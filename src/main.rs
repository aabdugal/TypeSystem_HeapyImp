use std::collections::HashMap;
use Exp::*;
use Stmt::*;
use Tp::*;

#[derive(Clone, PartialEq)]
pub enum Exp {
    Var(String),
    ReadHeap(String),
    Const(i32),
    BoolConst(bool),
    Add(Box<Exp>, Box<Exp>),
    Neg(Box<Exp>),
    Conj(Box<Exp>, Box<Exp>),
    Comp(Box<Exp>, Box<Exp>)
}

#[derive(Clone, PartialEq)]
pub enum Stmt {
    Assign(Box<Exp>, Box<Exp>),
    Update(Box<Exp>, Box<Exp>),
    Alias(Box<Exp>, Box<Exp>),
    New(Box<Exp>, Box<Exp>),
    Seq(Box<Stmt>, Box<Stmt>),
    Cond(Box<Exp>, Box<Stmt>, Box<Stmt>),
    Skip,
    While(Box<Exp>, Box<Stmt>)
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Tp{
    BoolVal(bool), NumVal(i32), Loc(i32)
}


fn typeCheckExp(exp: Exp, env: &HashMap<String, Tp>, locs: &HashMap<i32, Tp>) -> Option<Tp> {
    match exp {
        Const(x) => Some(NumVal(x)),
        BoolConst(x) => Some(BoolVal(x)),
        Add(x, y) => {
            let res1 = typeCheckExp(*x, &env, &locs);
            let res2 = typeCheckExp(*y, &env, &locs);
            match (res1, res2) {
                (Some(NumVal(xVal)), Some(NumVal(yVal))) => Some(NumVal(xVal+yVal)),
                _ => None
            }
        }
        // ReadStore(var) => env.get(&var).copied(),
        Var(var) => env.get(&var).copied(),
        ReadHeap(val)=> {
            if let Some(Loc(curLoc)) = env.get(&val).copied() {
                return locs.get(&curLoc).copied()
            }
            return None
        }
        Neg(x) => {
            match typeCheckExp(*x, &env, &locs) {
                Some(BoolVal(boolVal)) => return Some(BoolVal(!boolVal)),
                _ => None
            }
        }
        Conj(x, y) => {
            let res1 = typeCheckExp(*x, &env, &locs);
            let res2 = typeCheckExp(*y, &env, &locs);
            match (res1, res2) {
                (Some(BoolVal(xVal)), Some(BoolVal(yVal))) => if xVal && yVal {Some(BoolVal(true))} else {Some(BoolVal(false))},
                _ => None
            }
        }
        Comp(x, y) => {
            let res1 = typeCheckExp(*x, &env, &locs);
            let res2 = typeCheckExp(*y, &env, &locs);
            match (res1, res2) {
                (Some(NumVal(xVal)), Some(NumVal(yVal))) => {
                    if xVal <= yVal {
                        return Some(BoolVal(true))
                    }
                    return Some(BoolVal(false))
                }
                _ => None
            }
        }
    }
}

fn typeCheck(st: Stmt, mut env: HashMap<String, Tp>, mut locs: HashMap<i32, Tp>) -> (bool, HashMap<String, Tp>, HashMap<i32, Tp>) {
    let stCopy = st.clone();
    match st {
        Assign(var, val) => {
            if let Var(x) = *var {
                let evalVal = typeCheckExp(*val, &env, &locs);
                if evalVal.is_none() || env.contains_key(&x) {
                    return (false, env, locs)
                }
                env.insert(x, evalVal.unwrap());
                return (true, env, locs)
            }
            return (false, env, locs)
        }

        New(var, val) => {
            if let Var(x) = *var {
                let evalVal = typeCheckExp(*val, &env, &locs);
                if evalVal.is_none() || env.contains_key(&x) {
                    return (false, env, locs)
                }
                let curLoc = locs.keys().max().copied().unwrap_or(0)+1;
                locs.insert(curLoc, evalVal.unwrap());
                env.insert(x, Loc(curLoc));
                return (true, env, locs)
            }
            return (false, env, locs)
        }

        Update(var, val) => {
            if let Var(x) = *var {
                if let Some(Loc(curLoc)) = env.get(&x).copied() {
                    let evalVal = typeCheckExp(*val, &env, &locs);
                    if evalVal.is_none(){
                        return (false, env, locs)
                    }
                    // env.insert(x, Loc(curLoc));
                    locs.insert(curLoc, evalVal.unwrap());
                    return (true, env, locs)
                }
                return (false, env, locs)
            }
            return (false, env, locs)
        }

        Seq(st1, st2) => {
            let (res1, env1, locs1) = typeCheck(*st1, env, locs);
            if !res1 {
                return (false, env1, locs1)
            } 
            return typeCheck(*st2, env1, locs1)
        }
        Alias(var1, var2) => {
            match (*var1, *var2) {
                (Var(x), Var(y)) => {
                    if env.contains_key(&x) {
                        return (false, env,locs)
                    }
                    if let Loc(curLoc) = env.get(&y).copied().unwrap_or(BoolVal(false)) {
                        env.insert(x, Loc(curLoc));
                        return (true, env, locs)
                    }
                    return (false, env,locs)
                }
                _ => return (false, env,locs)
            }
        }
        Skip => (true, env, locs),
        Cond(cond, thenC, elseC) => {
            let evalVal = typeCheckExp(*cond, &env, &locs);
            match evalVal {
                Some(BoolVal(boolVal)) => {
                    if boolVal {
                        return typeCheck(*thenC, env, locs)
                    } else{
                        return typeCheck(*elseC, env, locs)
                    }
                }
                _ => return (false, env, locs)
            }
        }
        While(cond, exp) => {
            let evalCond = typeCheckExp(*cond, &env, &locs);
            match evalCond {
                Some(BoolVal(boolVal)) => {
                    if boolVal {
                        let (res1, env1, locs1) = typeCheck(*exp, env, locs);
                        if !res1 {
                            return (false, env1, locs1)
                        } 
                        return typeCheck(stCopy, env1, locs1)
                    }
                    return (true, env, locs)
                }
                _ => (false, env, locs)
            }
        }
    }
}
fn main() {
    let x = createVal(String::from("x"), Box::new(Const(0)));
    let ex = Seq(x, createVal(String::from("y"), Box::new(Var(String::from("l")))));
    println!("Result is {} ", typeCheck(ex, HashMap::new(), HashMap::new()).0);
}

fn createVal(var:String, exp: Box<Exp>) -> Box<Stmt> {
    return Box::new(Assign(Box::new(Var(String::from(var))), exp))
}
fn getVal(var: &str) -> Box<Exp> {
return Box::new(Var(String::from(var)))
}

fn getLocVal(var : &str, env : HashMap<String, Tp>, locs : HashMap<i32, Tp>) -> Tp{
    if let Loc(curLoc) = env.get(var).copied().unwrap_or(BoolVal(false)) {
        return locs.get(&curLoc).copied().unwrap()
    }
    return BoolVal(false);
}

fn generateTestVals() -> (HashMap<String, Tp>, HashMap<i32, Tp>) {
    // values: x = 3; y=-2; z = [1]; a = true; b = false; c = [true]
    let x = createVal(String::from("x"), Box::new(Const(3)));
    let y = createVal(String::from("y"), Box::new(Const(-2)));
    let z = Box::new(New(Box::new(Var(String::from("z"))), Box::new(Const(1))));
    let a = createVal(String::from("a"), Box::new(BoolConst(true)));
    let b = createVal(String::from("b"), Box::new(BoolConst(false)));
    let c = Box::new(New(Box::new(Var(String::from("c"))), Box::new(BoolConst(true))));
    let seq = Seq(Box::new(Seq(Box::new(Seq(Box::new(Seq(Box::new(Seq(a, b)), x)), y)), z)), c);
    let (res, env, locs ) = typeCheck(seq, HashMap::new(), HashMap::new());
    return (env, locs)
}
#[cfg(test)]
mod tests {

    use std::io::Read;

    use super::*;

    #[test]
    fn pass_parseExp() {
        let (env, locs) = generateTestVals();
        // passing asserts typeCheckExp
        assert_eq!(typeCheckExp(Var(String::from("x")), &env, &locs), Some(NumVal(3)));
        assert_eq!(typeCheckExp(Var(String::from("a")), &env, &locs), Some(BoolVal(true)));
        assert_eq!(typeCheckExp(ReadHeap(String::from("z")), &env, &locs), Some(NumVal(1)));
        assert_eq!(typeCheckExp(Const(0), &env, &locs), Some(NumVal(0)));
        assert_eq!(typeCheckExp(BoolConst(true), &env, &locs), Some(BoolVal(true)));
        assert_eq!(typeCheckExp(Add(Box::new(Var(String::from("x"))), Box::new(Var(String::from("y")))), &env, &locs), Some(NumVal(1)));
        assert_eq!(typeCheckExp(Neg(Box::new(Var(String::from("a")))), &env, &locs), Some(BoolVal(false)));
        assert_eq!(typeCheckExp(Conj(Box::new(Var(String::from("a"))), Box::new(BoolConst(true))), &env, &locs), Some(BoolVal(true)));
        assert_eq!(typeCheckExp(Conj(Box::new(Var(String::from("a"))), Box::new(Var(String::from("b")))), &env, &locs), Some(BoolVal(false)));
        assert_eq!(typeCheckExp(Comp(Box::new(Var(String::from("y"))), Box::new(Var(String::from("x")))), &env, &locs), Some(BoolVal(true)));
    }

    #[test]
    fn fail_parseExp() {
        let (env, locs) = generateTestVals();
        // missing val in env/locs
        assert_eq!(typeCheckExp(Var(String::from("none")), &env, &locs), None);
        assert_eq!(typeCheckExp(Add(getVal("none"), getVal("x")), &env, &locs), None);
        assert_eq!(typeCheckExp(ReadHeap(String::from("x")), &env, &locs), None);
        // different types
        assert_eq!(typeCheckExp(Add(getVal("x"), getVal("a")), &env, &locs), None);
        assert_eq!(typeCheckExp(Neg(getVal("x")), &env, &locs), None);
        assert_eq!(typeCheckExp(Conj(getVal("x"), getVal("a")), &env, &locs), None);
        assert_eq!(typeCheckExp(Comp(getVal("x"), getVal("a")), &env, &locs), None);
    }

    #[test]
    fn pass_parstStmt() {
        let (env, locs) = generateTestVals();
        // assertion that all values are in env as should be
        assert_eq!(env.get("x").copied(), Some(NumVal(3)));
        assert_eq!(env.get("y").copied(), Some(NumVal(-2)));
        assert_eq!(env.get("z").copied(), Some(Loc(1)));
        assert_eq!(locs.get(&1).copied(), Some(NumVal(1)));
        assert_eq!(env.get("a").copied(), Some(BoolVal(true)));
        assert_eq!(env.get("b").copied(), Some(BoolVal(false)));
        assert_eq!(env.get("c").copied(), Some(Loc(2)));
        assert_eq!(locs.get(&2).copied(), Some(BoolVal(true)));

        // 
        assert_eq!(typeCheck(Cond(Box::new(BoolConst(true)), Box::new(Skip), Box::new(Skip)), env.clone(), locs.clone()).0, true);
        let updateRes = typeCheck(Update(getVal("z"), Box::new(Const(0))), env.clone(), locs.clone());
        assert_eq!(updateRes.0, true);
        assert_eq!(updateRes.2.get(&1).copied(), Some(NumVal(0)));
        //testing alias and update tgthr
        let updateRes2 = typeCheck(Seq(Box::new(Alias(getVal("k"), getVal("c"))), Box::new(Update(getVal("k"), Box::new(BoolConst(false))))), env.clone(), locs.clone());
        assert_eq!(updateRes2.2.get(&2).copied(), Some(BoolVal(false)));
        //testing while
        let z_comp = Box::new(Comp(Box::new(ReadHeap(String::from("z"))), Box::new(Const(5))));
        let z_body = Box::new(Update(getVal("z"), Box::new(Add(Box::new(ReadHeap(String::from("z"))), Box::new(Const(1))))));
        let while_res = typeCheck(While(z_comp, z_body), env.clone(), locs.clone());
        assert_eq!(while_res.2.get(&1).copied(), Some(NumVal(6)));
        //testing conj and cond
        let test_conj = typeCheck(Cond(Box::new(Conj(Box::new(BoolConst(true)), Box::new(BoolConst(true)))), Box::new(Skip), Box::new(Skip)), env.clone(), locs.clone());
        assert_eq!(test_conj.0, true);
    }
    #[test]
    fn fail_parseStmt() {
        let (env, locs) = generateTestVals();
        // already existing val
        assert_eq!(typeCheck(Assign(getVal("x"), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(New(getVal("x"), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(Alias(getVal("x"), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        //alias should not contain var2
        assert_eq!(typeCheck(Alias(getVal("z"), getVal("x")), env.clone(), locs.clone()).0, false);
        //update should contain var1
        assert_eq!(typeCheck(Update(getVal("x"), Box::new(Const(0))), env.clone(), locs.clone()).0, false);

        // not variable passed
        assert_eq!(typeCheck(Assign(Box::new(Const(0)), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(New(Box::new(BoolConst(true)), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(Update(Box::new(Const(0)), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        

        // fail parseExp
        let expErr = Box::new(Var(String::from("none")));
        assert_eq!(typeCheckExp(*expErr.clone(), &env, &locs), None);
        assert_eq!(typeCheck(Assign(getVal("new"), expErr.clone()), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(New(getVal("new"), expErr.clone()), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(Update(getVal("x"), expErr.clone()), env.clone(), locs.clone()).0, false);
        
        // type check failing var
        assert_eq!(typeCheck(Assign(Box::new(Const(0)), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(New(Box::new(Const(0)), Box::new(Const(0))), env.clone(), locs.clone()).0, false);
        //var1 var2 should be vars
        assert_eq!(typeCheck(Alias(Box::new(Const(0)), Box::new(Const(0))), env.clone(), locs.clone()).0, false);

        //type check if val cannot be eval'd
        assert_eq!(typeCheck(Assign(getVal("p"), Box::new(ReadHeap(String::from("x")))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(New(getVal("p"), Box::new(ReadHeap(String::from("x")))), env.clone(), locs.clone()).0, false);
        assert_eq!(typeCheck(Alias(getVal("p"), Box::new(ReadHeap(String::from("x")))), env.clone(), locs.clone()).0, false);

        //test seq
        assert_eq!(typeCheck(Seq(Box::new(Assign(Box::new(Const(0)), Box::new(Const(0)))), Box::new(Assign(getVal("p"), Box::new(Const(0))))), env.clone(), locs.clone()).0, false);
        // test conds
        assert_eq!(typeCheck(Cond(Box::new(BoolConst(true)), Box::new(Update(getVal("x"), Box::new(Const(0)))), 
                            Box::new(Update(getVal("none"), Box::new(Const(0))))), env.clone(), locs.clone()).0, false);    
    }

    #[test]
    fn pas_big_case () {
        // x = 3; y=-2; z = [1]; a = true; b = false; c = [true]; d = z; 
        // if([c] && (neg a)) {
        //     while z <= x {
        //         d+=1
        //     }
        // } else {
        //     Skip;
        // }
        let (env, locs) = generateTestVals();
        let compSeq = Box::new(Comp(Box::new(ReadHeap(String::from("z"))), getVal("x")));
        let addToD = Box::new(Add(Box::new(ReadHeap(String::from("d"))), Box::new(Const(1))));
        let whileSeq = Box::new(While(compSeq, 
            Box::new(Update(getVal("z"), addToD))));
        let conjSeq = Box::new(Conj(Box::new(ReadHeap(String::from("c"))),
            Box::new(Neg(getVal("a")))));
        let condSeq = Box::new(Cond(conjSeq, 
                whileSeq, 
                Box::new(Skip)));
        assert_eq!(typeCheck(*condSeq, env.clone(), locs.clone()).0, true);
    }

}



