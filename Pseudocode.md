initialize empty stack
initialize empty list as history

while(eval)
    prompt user for input
    store input
    if input = quit -> exit loop
    
    index = input.length-1
    while(index >= 0)
        if(token at index is number) <- need to consider negatives, and multi digit numbers
            if(token at index-1 is '$') <- considering '$2' case, acessing history
                push onto stack -> history[input[index]]
                index-- <- aditional decrement to account for $
            else
                push onto stack input[index]
        else if(token at index is operator)
            pop off stack -> val1
            pop off stack -> val2 <- if this step fails there where too many operators, invalid expression
            if token is +
                push -> val1 + val2
            if token is -
                push -> val1 + negate(val2)
            if token is *
                push -> val1 * val2
            if token is /
                push -> val1 / val2
        else <- if token is not caught in the above cases, it's an invalid expression
            print invalid expression
        index--
    pop off stack -> result <- if there is more than one value left on the stack there were too many operands, invalid expression, check with peek
    display result
    add result to history array
    display history
    
