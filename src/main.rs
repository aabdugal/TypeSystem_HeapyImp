use std::collections::HashMap;
use Exp::*;
use Stmt::*;
use Tp::*;

#[derive(Clone)]
pub enum Exp {
    Var(String),
    Const(i32),
    BoolConst(bool),
    Add(Box<Exp>, Box<Exp>),
    Neg(Box<Exp>),
    Conj(Box<Exp>, Box<Exp>),
    Comp(Box<Exp>, Box<Exp>),
    ReadStore(String),
    ReadHeap(String)
}

#[derive(Clone)]
pub enum Stmt {
    Assign(Box<Exp>, Box<Exp>),
    Seq(Box<Stmt>, Box<Stmt>),
    Cond(Box<Exp>, Box<Stmt>, Box<Stmt>),
    Skip,
    While(Box<Exp>, Box<Stmt>),
    New(Box<Exp>, Box<Exp>),
    Update(Box<Exp>, Box<Exp>)
}

#[derive(Clone, Copy)]
pub enum Tp{
    BoolVal, NumVal, Loc(i32)
}


fn typeCheckExp(exp: Exp, env: &HashMap<String, Tp>, mut locs: &HashMap<i32, Tp>) -> Option<Tp> {
    match exp {
        Const(_) => Some(NumVal),
        BoolConst(_) => Some(BoolVal),
        Add(x, y) => {
            let res1 = typeCheckExp(*x, &env, &locs);
            let res2 = typeCheckExp(*y, &env, &locs);
            match (res1, res2) {
                (Some(NumVal), Some(NumVal)) => Some(NumVal),
                _ => None
            }
        }
        ReadStore(var) => env.get(&var).copied(),
        _ => Some(BoolVal)
    }
}

fn typeCheck(st: Stmt, mut env: HashMap<String, Tp>, mut locs: HashMap<i32, Tp>) -> (bool, HashMap<String, Tp>, HashMap<i32, Tp>) {
    match st {
        Assign(var, val) => {
            if let Var(x) = *var {
                let evalVal = typeCheckExp(*val, &env, &locs);
                if evalVal.is_none(){
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
                if evalVal.is_none() {
                    return (false, env, locs)
                }
                let curLoc = locs.keys().max().copied().unwrap_or(0);
                locs.insert(curLoc, evalVal.unwrap());
                env.insert(x, Loc(curLoc));
                return (true, env, locs)
            }
            return (false, env, locs)
        }

        Update(var, val) => {
            if let Var(x) = *var {
                if let Loc(curLoc) = env.get(&x).copied().unwrap_or(BoolVal) {
                    let evalVal = typeCheckExp(*val, &env, &locs);
                    if evalVal.is_none(){
                        return (false, env, locs)
                    }
                    env.insert(x, Loc(curLoc));
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
        Skip => (true, env, locs),
        _ =>  (false, env, locs)
    }
}
fn main() {
    let x = createVal(String::from("x"), Box::new(Const(0)));
    let ex = Seq(x, createVal(String::from("y"), Box::new(ReadStore(String::from("l")))));
    println!("Result is {} ", typeCheck(ex, HashMap::new(), HashMap::new()).0);
}

fn createVal(var:String, exp: Box<Exp>) -> Box<Stmt> {
    return Box::new(Assign(Box::new(Var(String::from(var))), exp))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pass_assignVal_eligVarName() {
        let ex = createVal(String::from("x"), Box::new(Const(0)));
        let (res, env2, _) = typeCheck(*ex, HashMap::new(), HashMap::new());
        assert_eq!(res, true);
        assert_eq!(env2.contains_key("x"), true);
    }

    #[test]
    fn fail_assignVal_notEligVarName() {
        let ex =Box::new(Assign(Box::new(Const(0)), Box::new(Const(0))));
        assert_eq!(typeCheck(*ex, HashMap::new(), HashMap::new()).0, false);
    }

    #[test]
    fn pass_readStore() {
        let x = createVal(String::from("x"), Box::new(Const(0)));
        let ex = Seq(x, createVal(String::from("y"), Box::new(ReadStore(String::from("x")))));
        assert_eq!(typeCheck(ex, HashMap::new(), HashMap::new()).0, true);
    }
    #[test]
    fn fail_readStore() {
        let ex = createVal(String::from("y"), Box::new(ReadStore(String::from("l"))));
        assert_eq!(typeCheck(*ex, HashMap::new(), HashMap::new()).0, false);
    }
    #[test]
    fn pass_Seq() {
        let x = createVal(String::from("x"), Box::new(Const(0)));
        assert_eq!(typeCheck(Seq(x, Box::new(Skip)), HashMap::new(), HashMap::new()).0, true);
    }
    #[test]
    fn fail_Seq(){
        let x = createVal(String::from("x"), Box::new(Const(0)));
        let ex = Seq(createVal(String::from("y"), Box::new(ReadStore(String::from("l")))), x);
        assert_eq!(typeCheck(ex, HashMap::new(), HashMap::new()).0, false);
    }

    // question: Is it possible to change the type that is stored in the heap? like loc 0 holds numerical, update it to bool?
    #[test]
    fn pass_newLoc_updateLoc() {
        let x = Box::new(New(Box::new(Var(String::from("x"))), Box::new(Const(0))));
        let res = typeCheck(Seq(x, Box::new(Skip)), HashMap::new(), HashMap::new());
        assert_eq!(res.0, true);
        assert_eq!(res.2.contains_key(&0), true);
        let y = Update(Box::new(Var(String::from("x"))), Box::new(Const(1)));
        let res2 = typeCheck(y, res.1, res.2);
        assert_eq!(res2.0, true);
        assert_eq!(res2.2.contains_key(&0), true);
    }

    #[test]
    fn fail_newLoc() {
        let x = Box::new(New(Box::new(Const(0)), Box::new(Const(0))));
        assert_eq!(typeCheck(Seq(x, Box::new(Skip)), HashMap::new(), HashMap::new()).0, false);
    }

    fn pass_typeCheckExp() {

    }


}



