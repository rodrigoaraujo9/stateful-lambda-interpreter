let fact =
    fix (lambda g . lambda x .
        if iszero x then 1 else (x * (g (x - 1)))
    )
in let catalan =
    lambda x .
        (fact (2 * x)) / ((fact (x + 1)) * (fact x))
in catalan 10
