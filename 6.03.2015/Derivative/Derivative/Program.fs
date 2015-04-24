type Const = int
type variable = char
type Sign = char
type Polynomial =
    | Constant of Const
    | Variable of variable
    | PolynomialExr of Polynomial * Sign * Polynomial

let rec countDer expression =
    match expression with
    | PolynomialExr(l, sign, r) -> 
        match sign with
        |'+'|'-' -> PolynomialExr(countDer l, sign, countDer r)
        |'*' -> PolynomialExr(PolynomialExr(countDer l, sign, r), '+', PolynomialExr(l, sign, countDer r))
        |'/' -> PolynomialExr(PolynomialExr(PolynomialExr(countDer l, '*', r), '-', PolynomialExr(l, '*', countDer r)), sign, PolynomialExr(r, '*', r))
        | _ -> Constant 0
    | Variable _ -> Constant 1
    | Constant _ -> Constant 0

let rec simplify expression =
    match expression with
    | Variable a -> Variable a
    | Constant b -> Constant b
    | PolynomialExr(l, sign, r) -> 
                    let left = simplify l
                    let right = simplify r

                    match left with
                    | Constant a -> 
                        match right with
                        | Constant b -> 
                                match sign with
                                |'+' -> Constant (a + b)
                                |'-' -> Constant (a - b)
                                |'*' -> Constant (a * b)
                                |'/' -> Constant (a / b)
                                | _ -> Constant 0
                        | _ -> 
                            if (a = 1 && sign = '*') || (a = 0 && (sign = '-' || sign = '+')) then
                                right
                            elif (a = 0 && (sign = '*' || sign = '/')) then
                                Constant 0
                            else
                                PolynomialExr(left, sign, right)
                    | _ ->
                        match right with
                        | Constant r -> 
                            if r = 0 && sign = '*' then
                                Constant 0
                            elif (r = 1 && (sign = '*' || sign = '/')) || (r = 0 && (sign = '-' || sign = '+')) then
                                left
                            else PolynomialExr(left, sign, right)
                        | _ -> PolynomialExr(left, sign, right)



(*let tree = PolynomialExr(PolynomialExr(Variable 'x', '*', Variable 'x'), '+', PolynomialExr(Constant 1, '*', Variable 'x'))
simplify (countDer tree)*)
