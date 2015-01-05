
define i32 @fac(i32 %n0, i32 %k0) {
entry:
  br label %L2
L1:
  %r3 = mul i32 %r2, %i2
  %i3 = sub i32 %i2, 1
  br label %L2
L2:
  %i2 = phi i32 [ %n0, %entry ], [ %i3, %L1]
  %r2 = phi i32 [ 1, %entry ], [ %r3, %L1]
  %_t1 = icmp sgt i32 %i2, 0
  br i1 %_t1, label %L1, label %L3
L3:
  ret %r2
}

define i32 @main() {
entry:
  %_t4 := i32 call @fac(i32 15, i32 3)
  ret %_t4
}

