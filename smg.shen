\*
I want a program that aids in the design of Verilog specifically for the
purposes of instruction decoding.  Hence, "Generator" is essentially a tool to
convert higher-level, or at least more convenient, descriptions of state
recognizers and desired actions into corresponding Verilog statements.  The
Verilog that is generated is intended to be portable across both commercial and
open-source Verilog tools, which puts a constraint on which features of Verilog
is used.  Therefore, we treat Verilog as a kind of assembly language: to the
greatest extent possible, we keep things to just AND, OR, XOR, NOT asynchronous
logic.  We don't even use always blocks.

Starting with a very simple example, let's look at the Verilog that implements
a simple a 3-bit Grey-code counter.  Here's the corresponding Verilog that we
want to generate.  For convenience, we split recognizers from intended output
generation.

	wire R0 = (ctr == 3'b000);
	wire R1 = (ctr == 3'b001);
	wire R2 = (ctr == 3'b010);
	wire R3 = (ctr == 3'b011);
	wire R4 = (ctr == 3'b100);
	wire R5 = (ctr == 3'b101);
	wire R6 = (ctr == 3'b110);
	wire R7 = (ctr == 3'b111);

	wire [2:0] out0 = R4 ? 3'b000 : 0;
	wire [2:0] out1 = R0 ? 3'b001 : 0;
	wire [2:0] out2 = R3 ? 3'b010 : 0;
	wire [2:0] out3 = R1 ? 3'b011 : 0;
	wire [2:0] out4 = R5 ? 3'b100 : 0;
	wire [2:0] out5 = R7 ? 3'b101 : 0;
	wire [2:0] out6 = R2 ? 3'b110 : 0;
	wire [2:0] out7 = R6 ? 3'b111 : 0;

	wire [2:0] next = |{out0,out1,out2,out3,out4,out5,out6,out7};

This is a lot of Verilog for something that you can more adequately describe in
a case or casez statement.  However, case statements produce priority encoders
when synthesized, meaning one and only one desired outcome will fire.  In this
simple example, it's not a big deal.  But, for more complex designs, you often
want a "multi-hot" decode logic, so you can re-use minterms across multiple
states.  This is how the 6502 instruction decoder works, for example.

Another reason to avoid large case statements is that it's all-too-easy to
accidentally forget to set an output, which causes latches to be synthesized
where one wasn't actually intended.  These latches might not harm operational
semantics of your circuit, but they do take up resources in an FPGA fabric.
For FPGAs as small as the Lattice iCE40-series parts, failing to remove these
latches can result in a design too big to fit.  I've tried setting "defaults"
in always-blocks and the like; however, these do not always work for me, and
I've occasionally observed differences in Xilinx and open-source tools like
Yosys.  Trying to predict when something will or will not work is at best an
art, and at worst an exercise in frustration.

Let's get back to our example.  Knowing the desired output, let's look at how
we can simply generate the above from idealized input in Shen S-expression
syntax.  Let's try this:

	[[on [[ctr 3'b000]] [next 3'b001]]
	 [on [[ctr 3'b001]] [next 3'b011]]
	 [on [[ctr 3'b011]] [next 3'b010]]
	 [on [[ctr 3'b010]] [next 3'b110]]
	 [on [[ctr 3'b110]] [next 3'b111]]
	 [on [[ctr 3'b111]] [next 3'b101]]
	 [on [[ctr 3'b101]] [next 3'b100]]
	 [on [[ctr 3'b100]] [next 3'b000]]]

We can store this example in a file called `grayctr` (no file extension for
convenience).  This is valid Shen code, so "loading" it implies evaluating it.
*\

(set grayctr (eval (head (read-file "grayctr"))))

\*
To get from this to the desired output, we need to apply a number of
transformations.  First, we need to extract all the recognizers, which itself
consists of generating Verilog wires and replacing the appropriate table
entries with the symbolic wire names corresponding to them.

	// Verilog output

	wire R0 = (ctr == 3'b000);
	wire R1 = (ctr == 3'b001);
	wire R2 = (ctr == 3'b011);
	wire R3 = (ctr == 3'b010);
	wire R4 = (ctr == 3'b110);
	wire R5 = (ctr == 3'b111);
	wire R6 = (ctr == 3'b101);
	wire R7 = (ctr == 3'b100);

	\\ Generator working state

	[[on R0 [next 3'b001]]
	 [on R1 [next 3'b011]]
	 [on R2 [next 3'b010]]
	 [on R3 [next 3'b110]]
	 [on R4 [next 3'b111]]
	 [on R5 [next 3'b101]]
	 [on R6 [next 3'b100]]
	 [on R7 [next 3'b000]]]

In actual output R0..R7 will be replaced by the result of (gensym R).

(filter-recognizers Rs) prints a list of Verilog wire definitions to the
stoutput stream, given a list of recognizers Rs.  It also returns a transformed
list of recognizer rules, with the conditions replaced by a symbolic wire name.
*\

(define filter-recognizers
	Recognizers ->
		(let WiresAndRules (map (function pull-recognizer) Recognizers)
		     Wires         (map (/. X (head X)) WiresAndRules)
		     Rules         (map (/. X (head (tail X))) WiresAndRules)
		     _             (map (function print-wire) Wires)
		     Rules))

\*
(print-wire Tokens) prints each token T in Tokens, each with a single space
afterward.  The result is ugly, but syntactically valid, Verilog.
*\

(define print-wire
	W ->	(let Strings (map (/. X (make-string "~A " X)) W)
		     (do (print-wire-h Strings) (nl))))

(define print-wire-h
	[T | []] -> (output T)
	[T | Ts] -> (do (output T) (print-wire-h Ts))
	X -> (error (make-string "print-wire-h: ?? ~A" X)))

\*
(pull-recognizer Rules) transforms each rule R in Rules into a list [A B],
where A contains a Verilog wire definition suitable for printing using
(print-wire), while B matches R except that the conditions for the rule are
replaced with the corresponding wire's name.  This lets subsequent stages deal
with the condition symbolically.
*\

(define pull-recognizer
	[on Conditions | Rules] ->
		(let Wire        (build-wire Conditions)
		     Name        (head (tail Wire))
		     Replacement (append [on Name] Rules)
		     [Wire Replacement])
	X -> (error (make-string "pull-recognizer: expected on-clause, got ~A" X)))

(define build-wire
	[C | Cs] -> (append [wire (gensym (protect R)) =] (and-terms C Cs) [;])
	[] -> (error "build-wire: Recognizers need at least one condition to check for.")
	X -> (error (make-string "build-wire: Got non-list argument ~A" X)))

(define and-terms
	C [] -> (and-term C)
	C [CC | CCs] -> (append (and-term C) [&] (and-terms CC CCs))
	X Y -> (error (make-string "and-terms: unknown arguments (~S ~S)" X Y)))

\*
(and-term X) attempts to create a Verilog equivalent term for X.

If X is a simple symbol S, then the Verilog form is |S, which means to
OR-all the bits of S together.  This implies that naming S alone, without
any further qualifiers, evaluates to true iff S is non-zero (e.g., just like
in C), regardless of how many bits represents S.

If X is a list of at least two elements, then the head of the list is the
signal we wish to check, while the tail (when appended together) is the
value to check against.  For example, given [ctr 3'b000], Shen will
interpret this into a list [ctr 3 'b000] (note: 3 elements).  Ultimately,
this will be massaged into a form [ctr == "3'b000"], which is more suitable
for output in a Verilog source listing.

Don't worry about the mixed types (symbols and strings, sometimes even
numbers).  They'll all be flattened into strings courtesy of (as-string) and
(join-string).
*\

(define and-term
	X            -> (and-term [X])  where (symbol? X)
	[LHS | []]   ->
		(let Term       (as-string LHS)
		     IsNegated  (= "~" (hdstr Term))
		     Name       (if IsNegated (tlstr Term) Term)
		     Format     (if IsNegated "~(|~A)" "(|~A)")
		     [(make-string Format Name)])
	[LHS | RHSs] -> ["(" LHS == (join-string (map (function as-string) RHSs)) ")"]
	X            -> (error (make-string "and-term: unknown term ~A" X)))

\*
(join-string ...) takes zero or more inputs, converts them to a string
representation, then joins them together with no gaps.  This overcomes flaws
with (@s), which if given only a single input yields a lambda expression,
and (cn), which under similar conditions also yields a lambda.
*\

(define join-string
	[] -> ""
	[X] -> (as-string X)
	[X | Xs] -> (cn (as-string X) (join-string Xs)))

\*
(as-string X) converts X to a string as best as Shen knows how.  This
function exists because I found myself using (make-string "~A" X) quite
frequently.
*\

(define as-string
	X -> (make-string "~A" X))

\*
After recognizers have been extracted, outputs need to be extracted as well.
Each output receives its own corresponding wire declaration.  At this point,
Generator will need to know how big the wire fields have to be.  A separate
list input to Generator can provide this information; e.g., [[next "[2:0]"]
...], but for now I just use Shen's (get) and (put) associative relation
functions.  E.g., (put next bus-spec "[2:0]") ought to do the trick.

	// Verilog output

	wire [2:0] out0 = R0 ? 3'b001 : 0;
	wire [2:0] out1 = R1 ? 3'b011 : 0;
	wire [2:0] out2 = R2 ? 3'b010 : 0;
	wire [2:0] out3 = R3 ? 3'b110 : 0;
	wire [2:0] out4 = R4 ? 3'b111 : 0;
	wire [2:0] out5 = R5 ? 3'b101 : 0;
	wire [2:0] out6 = R6 ? 3'b100 : 0;
	wire [2:0] out7 = R7 ? 3'b000 : 0;

	\\ Shen working state

	[[on R0 [next out0]]
	 [on R1 [next out1]]
	 [on R2 [next out2]]
	 [on R3 [next out3]]
	 [on R4 [next out4]]
	 [on R5 [next out5]]
	 [on R6 [next out6]]
	 [on R7 [next out7]]]

The outX signals aren't in the same order as our hypothetical, hand-written
example, but that's of no consequence.  As long as the signals are uniquely
identified, and Generator properly accounts for them, things should just work.
outX symbols are similarly generated with (gensym out).

Given a list of the form [on Rn [O1 V1] [O2 V2] ...], we want to generate
Verilog of the form "wire [...] out100 = Rn ? V1 : 0;".  To keep track of which outN
wires correspond to which Ox symbols the user provided, we also need to record
out100 (and friends) in a (label * (list symbol)) mapping.
*\

(define filter-outputs
	WorkingState -> (map (function filter-output) WorkingState))

(define filter-output
	[on Rule | Outputs] ->
		(let Replacement (alloc-ids [on Rule | Outputs])
		     Labels      (tail (tail Replacement))
		     Plan        (plan-wires Rule Outputs Labels)
		     _           (map (function print-wire) Plan)
		     Replacement))

(define plan-wires
	Rule Assignments Labels -> (parallel-map (/. A L (plan-wire Rule A L)) Assignments Labels))

(define plan-wire
	Rule Assignment Label -> (wire-from-output-terms (head Assignment) (head (tail Label)) Rule (tail Assignment)))

(define wire-from-output-terms
	Output Id Rule Terms ->
		[wire (bus-spec Output) Id = Rule ? (join-string Terms) : 0 ;])

(define bus-spec
	X -> (trap-error (get X bus-spec) (/. X "")))

(define parallel-map
	_ [] [] -> []
	_ [] _ -> (error "parallel-map: list length mismatch")
	_ _ [] -> (error "parallel-map: list length mismatch")
	Fn [L1 | L1s] [L2 | L2s] -> [(Fn L1 L2) | (parallel-map Fn L1s L2s)])

(define alloc-ids
	[on Rn | Outputs] -> [on Rn | (map (function alloc-id) Outputs)])

(define alloc-id
	[OutSym | _] -> [OutSym (gensym out)])

\*
The final step is to transpose the matrix, so to speak, and extract out all the
terms targeting next:

	\\ Shen working state

	[[on R0]
	 [on R1]
	 [on R2]
	 [on R3]
	 [on R4]
	 [on R5]
	 [on R6]
	 [on R7]]

	[next [out0 out1 out2 out3 out4 out5 out6 out7]]

At this point, we generate our wire-OR sequence:

	wire [2:0] next = |{out0,out1,out2,out3,out4,out5,out6,out7};

After this step, we filter out all the empty on-clauses, leaving us just with:

	[]

When we get to this point, we know we're done, and the Verilog should be
correct.
*\
