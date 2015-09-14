simpatico
=========

A C source code style checker..

## What is it?

This style marker should enforce the rules outlayed in
the 'csse2310-style-guide.pdf' which was created for the computer
science course 'CSSE2310' at the University of Queensland.

## Current method

Simpatico tokenises the file, then uses a modified recursive descent parser to
step through the code and check qualitative issues. Headers are processed
recursively, with system headers being pre-processed for types for simplicity.

After the automarker is complete the course tutors must go
through the generated errors to validate the process and catch any qualitative
errors.

## Error Format

Each style error must be declared in the format:
    filename:lineNumber: [CATEGORY] Description

The categories are roughly described below. More details are given in
`rules.md` and in `csse2310-style-guide.pdf`.

### NAMING
- variables
- defines
- functions
- typedefs

### BRACES
- space before brace
- correct placement
- correct alignment

### INDENTATION
- multiples of four spaces
- nesting correctly indented
- line continuation

### WHITESPACE
- grammatical spacing around assignment operators
- correctly spaced vertically

### COMMENTS
- globals
- functions (parameters esp.)
- lengthy or tricky code

### OVERALL
- no function over 50 lines
- modularity / no excessive duplication of code

### LINE-LENGTH
- all lines must be shorter than 80 chars long (including \r)

## Usage

```
./simpatico.py file1.c file2.c ...

```
