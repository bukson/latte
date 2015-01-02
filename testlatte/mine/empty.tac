Constant C1: abcdef
Constant C2: ghij

fun train:
entry:
	$a1 := 3
	$b1 := $a1
	$a2 := $b1
	param $b1
	param $b1
	param $c0
	$_t1 := call main, 3
	$_t2 := C1 + C2
	$d1 := $_t2
	$_t3 := 1 - $c0
	$_t4 := $_t3 || $c0
	$_t5 := 1 - $_t4
	$c1 := $_t5
	$_t6 := $a2 <= $b1
	$c2 := $_t6
	$_t7 := 2 * $a2
	$a3 := $_t7
	$_t8 := $a3 <= $b1
	$c3 := $_t8
	$_t9 := $a3 + 3
	param $_t9
	param $a3
	param $c3
	$_t10 := call main, 3
	$a4 := $_t10
	return

fun main:
entry:
	if $c0 <= 0 then goto L2 else goto L1
L1:
	param $a0
	$_t11 := call printInt, 1
	goto L2
L2:
	if $a0 <= 0 then goto L6 else goto L7
L7:
	if $b0 > 0 then goto L3 else goto L6
L6:
	if $a0 >= 0 then goto L4 else goto L8
L8:
	if $b0 >= 0 then goto L4 else goto L3
L3:
	return 9
	goto L5
L4:
	return 3
	goto L5
L5:

fun fac:
entry:
	$i1 := $n0
	$r1 := 1
	goto L10
L9:
	$_t12 := $r1 * $i1
	$r2 := $_t12
	goto L10
L10:
	$r3 = fi [(entry,$r1),(L9,$r2)]
	if $i1 > 0 then goto L9 else goto L11
L11:
	return $r3

