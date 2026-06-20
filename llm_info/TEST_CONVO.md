A compiler is the ultimate example of a data pipeline. You have a massive stream of text that gets transformed into an Abstract Syntax Tree (AST), which then gets mutated, annotated with types, lowered into an Intermediate Representation (IR), and finally emitted as assembly or machine code.

If you try to unit test a compiler by writing strings of source code, compiling them all the way down, and checking the execution output, you aren't writing unit tests—you are writing full integration tests. While those are necessary, they are slow and make it incredibly difficult to isolate bugs in specific phases.

Here is how you inject true unit tests into the different phases of a compiler architecture.

### 1. The Frontend: Testing the Parser

The parser takes raw text and builds the massive data structure (the AST). The "Imperative Shell" here is the file system reader. You want to bypass that entirely for unit tests.

Instead of testing `compile_file("main.src")`, you inject tests directly at the string-to-AST boundary.

```python
# The pure function
def parse_expression(source: str) -> ASTNode:
    lexer = Lexer(source)
    parser = Parser(lexer)
    return parser.parse_expr()

def test_binary_operation_parsing():
    # Pass a tiny, isolated string
    ast = parse_expression("x + 5")
    
    # Verify the exact shape of the tree, no file I/O needed
    assert isinstance(ast, BinaryOpNode)
    assert ast.operator == "+"
    assert ast.left.name == "x"
    assert ast.right.value == 5

```

### 2. The Middle-end: Testing Static Analysis & Type Checking

This is usually the hardest part to test because static analysis often requires traversing the AST while maintaining a massive, mutable state: the Environment (or Symbol Table).

If you are dealing with complex type theory—like verifying capability rules or viewpoint adaptation in an actor-based concurrency model—you cannot afford to set up a whole program just to test one assignment rule.

**The Injection Strategy:** Pass the specific AST node and a hand-crafted, minimal Environment directly to the visitor function.

```python
def test_viewpoint_adaptation_assignment():
    # 1. Setup a minimal, fake environment (the injection)
    env = SymbolTable()
    env.define("actor_ref", Type.ISO)
    env.define("local_val", Type.VAL)

    # 2. Construct just the AST nodes involved in the assignment
    # e.g., local_val = actor_ref
    assign_node = AssignmentNode(
        target=Identifier("local_val"), 
        value=Identifier("actor_ref")
    )

    # 3. Test the pure type-checking logic in isolation
    checker = TypeChecker(env)
    result = checker.visit_assignment(assign_node)
    
    # 4. Assert the semantic rules held up
    assert result.is_valid == False
    assert "Cannot alias isolated reference" in result.error_message

```

### 3. The Backend: Testing Code Generation

When your compiler finally lowers the AST or IR into target instructions, you want to test the instruction selection logic without actually running an emulator.

The key here is to have your code generator yield instruction objects rather than immediately writing text to a file. You can then unit test that the correct opcodes and operand orders are generated.

```python
def generate_binary_op(node: BinaryOpNode, allocator: RegisterAllocator) -> list[Instruction]:
    dest_reg = allocator.get_free_reg()
    src1_reg = allocator.get_reg_for(node.left)
    src2_reg = allocator.get_reg_for(node.right)
    
    if node.operator == "+":
        # Target architecture assumption: destination is the first operand
        return [Instruction("add", dest_reg, src1_reg, src2_reg)]

def test_mips_addition_emission():
    # Setup dummy dependencies
    mock_allocator = FixedRegisterAllocator(dest="$t0", left="$t1", right="$t2")
    node = BinaryOpNode("+", left=Identifier("x"), right=Identifier("y"))
    
    # Generate the instruction list
    instructions = generate_binary_op(node, mock_allocator)
    
    # Verify the MIPS-like emission order is correct (dest first)
    assert len(instructions) == 1
    assert str(instructions[0]) == "add $t0, $t1, $t2"

```

### 4. Golden Tests (Snapshot Testing)

While the above methods are pure unit tests, compilers benefit heavily from "Golden Tests" for the pipeline transitions.

Instead of writing hundreds of `assert` statements to check every branch of an AST, you run a test file through a specific phase (e.g., lowering to IR), serialize the output to a text file, and commit it as the "Golden" result. On the next test run, the test harness generates the IR again and diffs it against the Golden file. If it changes, the test fails, and you must manually inspect the diff to decide if the change was intentional or a regression.

---

Which specific phase of your compiler—parsing, static analysis, or instruction emission—is currently creating the most friction when you try to isolate it for testing?