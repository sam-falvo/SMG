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

	assign next = out0|out1|out2|out3|out4|out5|out6|out7;

This is a lot of Verilog for something that you can more adequately describe in
a case or casez statement.  However, case statements produce priority encoders
when synthesized, meaning one and only one desired outcome will fire.  In this
simple example, it's not a big deal.  But, for more complex designs, you often
want a "multi-hot" decode logic, so you can re-use minterms across multiple
states.  This is how the 6502 instruction decoder works, for example.

(Aside: why not use next = |{out0,out1,out2...}?  Because, {out0,out1,out2,...}
evaluates to a concatenation of these outputs into a single bit-string, and
then |{...} ORs them all down to a single bit.  That's not what we want; we
want word-wise ORing of the outputs.)

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
The final step is to print out our assignments.  Here, we generate our wire-OR
sequences.  For example:

	assign next = out0|out1|out2|out3|out4|out5|out6|out7;
*\

(define filter-bindings
	WorkingState -> (for-each-index (function print-assignments-in-bucket) (vector-of-bindings WorkingState)))

\*
To accomplish this, we in effect transpose what's left of our working state,
thus associating all wires assigned to a given output and keeping them together
for convenient iteration.  It's a relatively simple matter to go from this:

	[next [out0 out1 out2 out3 out4 out5 out6 out7]]

into an assignment statement such as you see in the previous chunk.

The strategy is simple enough.  For each row in what's left of our working
state, we process all the remaining output bindings.  For each binding we find,
we append the wire name to a list dedicated just for that output.  Putting this
into concrete terms, if we have the following structure:

	[[on R0 [next out0] [irq out1]]
	 [on R1 [next out2]]
	 [on R2 [next out3] [irq out4]]
	 [on R3 [next out4]]
	 [on R4 [next out5] [eoi out6]]]

After stepping through the first element of the list, we discover next and irq
as outputs, so our association database looks (logically!) like:

	[[[next bindings] out0] [[irq bindings] out1]]

After the next row is processed, we only find next, so our state becomes:

	[[[next bindings] out2 out0] [[irq bindings] out1]]

and so on through the rest of the rows.  After all is done with, we should end
up with an association structure like this:

	[[[next bindings] out5 out4 out3 out2 out0]
	 [[irq bindings] out4 out1]
	 [[eoi bindings] out6]]
*\

(define vector-of-bindings
	WorkingState ->
		(let V (vector 100)
                     _ (map (/. X (remember-bindings X V)) WorkingState)
		     V))

(define remember-bindings
	[on _ | Bindings] V -> (map (/. X (remember-binding X V)) Bindings))

(define remember-binding
	[Output Wire] V ->
		(let Contributions (trap-error (get Output bindings V) (/. X []))
		     (put Output bindings [Wire | Contributions] V)))

\*
Once we have the associations built-up, we are free to output the corresponding
Verilog wire-OR statements.  Note that these outputs are assumed to be module
outputs, and declared in the module header.  Therefore, although technically
wires, we use the assign statement instead.  It's a Verilog thing.

We start by creating a procedure to call a function on a vector element if it
exists.
*\

(define for-each-index
	Fn V -> (for-each-index-h Fn V 1 (+ 1 (<-address V 0))))

(define for-each-index-h
	_ _ Max Max -> _
	Fn V Index Max -> (do (trap-error (Fn (<-vector V Index)) (/. X []))
					  (for-each-index-h Fn V (+ 1 Index) Max)))

\*
Once we can iterate over all the vector elements cleanly, we then want to
iterate over the contents of a vector element.  Since we use (put) to construct
our associations, it treats each vector as a hash bucket, multiple bindings can
sit at a single element.  Thankfully, Shen uses lists for this, so iterating is
a simple matter of (map)'ing.
*\

(define print-assignments-in-bucket
	Bucket -> (map (function print-assignment) Bucket))

\*
An assignment takes the following form.  Note we don't have to specify a bus
width; if it's required, it'll be part of the module's declaration:
*\

(define print-assignment
	[[Output Label] | Terms] -> (output (make-string "assign ~A = ~A;~%" Output (ored Terms))))

(define ored
	[X | []] -> (as-string X)
	[X | Xs] -> (make-string "~A|~A" (as-string X) (ored Xs)))

\*
To produce the complete listing, use the following statement:

	(verilog-from-sm (value grayctr))
*\

(define verilog-from-sm
	WorkingState -> (filter-bindings (filter-outputs (filter-recognizers WorkingState))))

