# LParen
A socially-aware programming language written in F#.

This is a hobby project that I documented on a series of blogs:

- [A Simple Computation Engine in F#](https://isthisit.nz/posts/2022/a-simple-computation-engine-in-fsharp/) - S-Expression calculator
- [Implementing General Computation](https://isthisit.nz/posts/2022/implementing-general-computation/) - Variables & functions
- [Abstractions for Logic](https://isthisit.nz/posts/2022/abstractions-for-logic/) - Booleans & logical operators.
- [Code Execution: Eval, Apply](https://isthisit.nz/posts/2023/code-execution-eval-apply/) - Functions & lexical environments.


# Run
REPL: 

    dotnet run --project src/LParen.Exe

    >> (define fib 
        (lambda (n)
          (cond ((= n 0) 0)
                ((= n 1) 1)
                (true (+ (fib (- n 1))
                          (fib (- n 2)))))))
        
    >> (fib 35)
    9227465

Execute a file:

    dotnet run --project src/LParen.Exe -- --file src/LParen.Exe/Examples/fibonacci-numbers.scm

# Web
This project cross-compiles to Javascript using [Fable](https://fable.io/). Requires NodeJS installed locally. Run the REPL in your web browser by

    cd fable
    npm ci
    npm start


