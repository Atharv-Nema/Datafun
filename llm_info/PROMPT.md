The goal of this project is to implement a compiler for datafun, with very rigorous testing.

The syntax and the typing rules are given in syntax.png and typing_rules.png
A previous instance of claude code has unsuccessfully worked on this. The remanants of the code are still present.
Your job is to modify it, correct it, and make it better.

General guidelines:
1. I want to keep you on a very short leash. Testing is very important to me. Ideally every (important) function should
come with clear and concise documentation and with associated tests. I want you to first plan all the black-box functions
you want first, figure out how to test it, write tests for it, and only then should you implement it (so a top-down approach)
2. I have contents of a conversation with gemini about testing in llm_info/TEST_CONVO.md. Please refer to it/modify it as you see fit
(TEST_CONVO.md serves only as a very rough starting prior. I expect that you will modify it to make it more rigorous)

Task 1: Parser + typing rules
llm_info/syntax.png and llm_info/typing_rules.png have the typing rules for datafun.
I want you to implement the parsing and the typing rules. Previous claude instance has already implemented stuff, and I strongly
suspect that it is correct. Your tasks are to modify the code according to "general guidelines" (main task is to inject tests).

Task 2: Come up with a typeless intermediate representation and an interpreter for it (to be refined by the human)

Task 3: Come up with seminaive evaluation to interpret it (to be refined by the human)