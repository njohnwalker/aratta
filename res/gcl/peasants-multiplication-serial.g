
/*
method pm(x : int, y : int) returns (res: int)
  requires x >= 0 && y >= 0
  ensures res == x * y
{
  var a := x;
  var b := y;
  res := 0;
  while (a > 0)
    invariant a * b + res == x * y
    invariant a >= 0 && b >= 0 && x >= 0 && y >= 0
  {
    if a % 2 == 1 {
      res := res + b;
    }
      a := a / 2;
      assert a >= 0;
      b := b * 2;
  }
  return res;
}
*/

@req(x >= 0 & y >= 0)
a, b, res := x, y, 0 ;

// invariant a * b + res == x * y

do @inv()
[] a > 0 & a % 2 == 1 ->
    res := res + b ;
    a,b := a / 2, b * 2
[] a > 0 & ~ a % 2 == 1 ->
    a,b := a / 2, b * 2  
od
@ens(res == x * y)
