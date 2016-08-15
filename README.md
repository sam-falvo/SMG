# SMG

State Machine Generator, or SMG, is a tool I wrote for two reasons:

1.  I wanted a relatively simple project with which to learn the Shen programming language, and,
2.  I wanted a tool which would help me complete various components of my Kestrel Computer project.

## Problem Statement

Verilog, as mature as the language is,
still does not provide any convenient mechanism
to implement even a moderately complex state machine.

One could implement a state machine using `case` or `casez` statements,
but these always synthesize as priority encoders,
which takes up more resources than simple pattern-matching AND gates.
They also prevent re-use of decode logic.
For example, the 6502 breaks accumulator instructions into two segments:
the low five bits are processed by operand-fetch logic in the state machine,
for they determine the addressing mode to use,
while the top three bits determines which ALU function to perform on the accumulator.
Both of these chunks of logic operate at the same time,
communicating with each other as required.
When the time is right,
the 6502 will also engage its instruction fetch logic as well.
This is one of the reasons why the 6502 is so time-efficient in its instruction decode and execution.
It's also why it's so *small*,
weighing in at [approximately 3510 transistors](https://en.wikipedia.org/wiki/Transistor_count).

One could break the case logic up into multiple case statements,
but again, this poses practical problems.
The biggest consternation for me is that
related logic is now scattered across the source file.
The bigger the design, the more difficult it is to maintain intellectual control over the project.

All too often, I forget to set all outputs in each case declared.
The result is unanticipated synthesis of latches.
Ordinarily, this isn't *too* big of an issue;
but, again, unanticipated latches consumes FPGA resources.
For small parts, like iCE40-HX4K FPGAs,
unanticipated resource consumption can lead a relatively simple design to not fit on the part.

Wait a minute, I hear you proclaim: you can use defaults in `always` blocks and as well in case statements!
In my personal experience,
with the full disclosure that I do not work as a professional chip designer,
I've found this doesn't universally work, and it's hard to know when it won't.
I've run into problems with tool support for things like setting signal defaults in `always`-blocks.
Sometimes, in my experience at least, they work; and sometimes they generate Verilog synthesis errors.
This is not just a Yosys-thing either; I find similar behaviors with Xilinx WebPACK ISE too.

Between unanticipated latches and undesirable priority encoder logic in the best of cases,
and inconsistent synthesis/simulation support in developer toolchains at the worst,
it seems the only thing that *does* work consistently and portably is asynchronous AND, OR, and inverter logic.
Priority decoders are great; but they belong in an interrupt controller.
They have virtually no place inside the instruction decode logic of a CPU,
or in the state machine for an SDRAM controller.

## Solution Statement

SMG is designed to take a state-machine description of a circuit,
expressed in a more tabular format instead of an algorithmic format,
and translate it into a purely asynchronous logic Verilog module.
Other modules are responsible for providing the stateful elements that feeds the SMG-generated logic.

# Examples

t.b.d. since we're still under development.
For now, you can always check the source file `smg.shen`,
where I use literate programming techniques to illustrate why things are done as they are.
Examples are provided where appropriate.

# About the Code

The code is written in a [literate style](http://literateprogramming.com)
using a [Lisp dialect called Shen.](http://shenlanguage.org/)
I used Shen for the following reasons:

* **Syntax matters.**  You might revile the S-expression syntax used by Shen and the input files consumed by SMG; however, even a simple example like a 3-bit Gray-code counter illustrates the suitability of S-expressions to the task.  This isn't even considering macro support opportunities.
* **Macro support.**  Since Shen is a Lisp-family language, it will have exceptional macro support.  Verilog supports a C-preprocessor-like thing which is, quite strangely, *even less powerful* than the C preprocessor.  Providing the macro capabilities of Lisp allows for potentially much more expressive designs.
* **Education, advancement, and personal development.** I wanted a real-world problem to solve using Shen, in order to really learn the language and its capabilities.
* **Opportunity for future enhancement.**  I can envision, someday, someone, somewhere will run into problems too large for the relatively low-level and simplistic input consumed by SMG.  Or, perhaps, they'll find a use-case where s-expressions are ill-suited.  Instead of going off and writing their own tool, perhaps they will elect to enhance SMG instead, building on top of what SMG already brings to the table.  Shen offers a built-in Prolog engine for logic programming which could be used as an expert system to help design sophisticated logic concisely, and as well offers a built-in Yacc-like parser, which helps support more sophisticated syntaxes.  I envision tools built with these features would exist as front-end filters to SMG.

## See a bug?  Want to contribute?

I follow the [Collective Code Contribution Contract (C4)](http://rfc.zeromq.org/spec:42/C4/)
to the greatest extent that this tiny project allows.
If you want to change anything, be it code or documentation,
please feel free to fork the project, apply your changes, and open a pull-request.
I will happily commit enhancements and bug-reports.
(As a matter of good etiquette, though,
please link your PR to an open Github issue stating the problem with SMG.
If none exists, please create one.
That way, we have a historical record
of *why* commits were applied
in a format more convenient than a git commit history.)

