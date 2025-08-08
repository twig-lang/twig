extern "C" function rand: i32;

function sum_rands: i32 = {
 let left = unsafe rand;
 let right = unsafe rand;

 left + right
};
