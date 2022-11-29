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
    Comp(Box<Exp>, Box<Exp>)
}

#[derive(Clone)]
pub enum Stmt {
    // TODO: change box<Exp> to variable later somehow. For now handled in typeCheck
    Assign(Box<Exp>, Box<Exp>),
    Seq(Box<Stmt>, Box<Stmt>),
    Cond(Box<Exp>, Box<Stmt>, Box<Stmt>),
    Skip,
    Break,
    While(Box<Exp>, Box<Stmt>),
    New(Box<Exp>, Box<Exp>),
    Update(Box<Exp>, Box<Exp>)
}


#[derive(Clone)]
pub enum Tp{
    BoolVal, NumVal, Loc(i32)
}


// Question: env probably should be <String, Tp> just to denote what it stores 

fn typeCheckExp(exp: Exp, env: &HashMap<String, Tp>, mut locs: &HashMap<i32, Tp>) -> Option<Tp> {
    match exp {
        Const(_) => Some(NumVal),
        BoolConst(_) => Some(BoolVal),
        _ => Some(BoolVal)
    }
}

fn typeCheck(st: Stmt, mut env: HashMap<String, Tp>, mut locs: HashMap<i32, Tp>) -> bool {
    match st {
        Assign(var, val) => {
            if let Var(x) = *var {
                let evalVal = typeCheckExp(*val, &env, &locs);
                if(!evalVal.is_none()){
                    return false
                }
                env.insert(x, evalVal.unwrap());
            }
            return false
        }
        New(var, val) => {
            if let Var(x) = *var {
                let evalVal = typeCheckExp(*val, &env, &locs);
                if(!evalVal.is_none()){
                    return false
                }
                let curLoc = locs.keys().max().unwrap_or(&0).clone();
                locs.insert(curLoc, evalVal.unwrap());
                env.insert(x, Loc(curLoc));
                return true;
            }
            return false
        }



        _ => true
    }
}
fn main() {
    let ex = Assign(Box::new(Var(String::from("x"))), Box::new(Const(0)));
    // let ex1 = Stmt::Seq(Exp::Const(0), Skip);

    println!("Result is {:?} ", typeCheck(ex, HashMap::new(), HashMap::new()));
}

// fn eval(mut sigma: HashMap<String, i32>, b : ImpStmt) -> (HashMap<String, i32>, Signal) {
//     let bcopy = b.clone();
//     match b {
//         ImpStmt:: Assign(x, a) => {
//             sigma.insert(x, aeval(&sigma, *a));
//             (sigma, Signal::Continue)
//         }
//         ImpStmt:: Seq(x, y) => {
//             let (sigma1, sig1) = eval(sigma, *x);
//             if let sig1 = Signal::Break {
//                 return (sigma1, sig1);
//             }
//             eval(sigma1, *y)
//         }
//         ImpStmt::Cond(cond, x, y) => {
//             if beval(&sigma, *cond) {
//                 eval(sigma, *x)
//             }else{
//                 eval(sigma, *y)
//             }
//         }
//         ImpStmt::Skip => (sigma, Signal::Continue),
//         ImpStmt::Break => (sigma, Signal::Break), 
//         ImpStmt:: While(cond, x) => {
//             if beval(&sigma, *cond) {
//                 let (sigma1, sig) = eval(sigma, *x);
//                 if let sig = Signal::Break {
//                     (sigma1, Signal::Continue)
//                 }
//                 else{
//                     eval(sigma1, bcopy)
//                 }
//             }
//             else{
//                 (sigma, Signal:: Continue)
//             }
//         }
//     }
// }



