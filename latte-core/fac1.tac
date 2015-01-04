
fun fac:
entry:
$i1 := $n0
$r1 := 1
goto L2
L2:
$i2 = TMPfi i[(entry,$i1),(L1,L1)]
$n1 = TMPfi n[(entry,$n0),(L1,L1)]
$r2 = TMPfi r[(entry,$r1),(L1,L1)]
if $i2 > 0 then goto L1 else goto L3
L1:
$x1 := 3
$_t1 := $r3 * $i2
$r3 := $_t1
$_t2 := $i3 - 1
$i3 := $_t2
goto L2
Dump fromList [("i",$i3),("n",$n1),("r",$r3),("x",$x1)]
ChangeBlocs
L3:
return $r3

fun if1:
entry:
if $a0 >= 0 then goto L5 else goto L4
L4:
$_t3 := 0 - 7
$b1 := $_t3
goto L5
L5:
$b2 = fi [(entry,$b0),(L4,$b1)]
return $b2

fun if2:
entry:
if $a0 >= 0 then goto L7 else goto L6
L6:
$_t4 := 0 - 7
$b1 := $_t4
goto L8
L7:
$b2 := 42
goto L8
L8:
$b3 = fi [(L6,$b1),(L7,$b2)]
return $b3

fun main:
entry:
return 0

fun foo:
entry:
$xx1 := 5
$_t5 := $b0 + $c0
$a1 := $_t5
$b1 := $a1
$a2 := $b1
$_t6 := 2 * $a2
$b2 := $_t6
$_t7 := $a2 + $b2
return $_t7

