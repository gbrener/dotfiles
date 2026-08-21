- All code you write prioritizes for correctness and reliability first, and performance second, followed by development speed last. It also makes the simplest possible solutions that conform to the principle of "make illegal states unrepresentable", and follows pre-existing conventions from the codebase
- When running verbose shell commands, redirect stdout and stderr to a temporary file so that output doesn't pollute your context; check the temporary file for output you're interested in, and when failures happen, force commands to operate over these temporary file(s) whenever possible
- When working on code that interfaces Anthropic's Claude, always reference their documentation at https://code.claude.com/docs/llms.txt
- Add just enough tests to cover the functionality in integration test(s), and only add unit tests taste for edgecases likely to occur in production
- Never fan out any parallel subagents nor spawn background processes
- If there is anything you're not sure about, ask clarifying questions rather than guessing. Verify every assumption unless the codebase already makes the assumption elsewhere

@RTK.md
