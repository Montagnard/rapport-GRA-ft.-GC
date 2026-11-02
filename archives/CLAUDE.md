# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Security Rules
- You are strictly confined to the current directory.
- Never attempt to read or write files outside of this folder.
- Do not use absolute paths (e.g., C:\...).
- If a task requires accessing external data, ask the user for permission or to provide the file.

## Project Overview

We are the think tank Global Redistribution Advocates and write a report destined to the EU Council to propose credible and acceptable new own resources for the EU budget. The report's main text is 6-page and it contains 6 appendices:
- Luxury taxation
- Aviation taxation
- Wealth taxation
- Financial Transactions Tax
- Fossil fuel profit tax
- Digital services tax

## Workflow / Pipeline



## Session Startup


## Files

Read all files when you open a session.

- `references/` contains papers that are useful to write the report or an appendix
- `annexes/` contains each appendix

## Code Style

- Use `snake_case` for all variable and function names.
- Always use the native pipe `|>` (R 4.1+), never `%>%`.
- Prefer compact, single-line expressions where readable.
- Document functions with roxygen2 style.
- Explicitly handle `NA` values; distinguish "Don't know" from "Refusal".

## Key Rules

- Never read or modify `.RData` files or any file listed in `.gitignore`.
- Before writing 500+ lines of code, provide a summary of the logic first.
- After completing a TODO item, tick its checkbox in `TODO.md`.
- Don't compile .tex files in `/[folder]` but in `/[folder]/build/`: there should be no auxiliary files in `//[folder]`.
