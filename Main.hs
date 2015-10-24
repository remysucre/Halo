import Grammar

main = getContents >>= print . sentence . lexer
