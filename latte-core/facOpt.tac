
fun main:
entry:
	$i1 := $n0
	$r1 := 1
	goto L2
L1:
	$_t1 := $r2 * $i2
	$_t2 := $_t1 * $i2
	$r3 := $_t2
	$_t3 := $i2 - 1
	$i3 := $_t3
	goto L2
L2:
	$i2 = fi [(entry,$i1),(L1,$i3)]
	$n1 := $n0
	$r2 = fi [(entry,$r1),(L1,$r3)]
	if $i2 > 0 then goto L1 else goto L3
L3:
	$_t4 := $n1 + 3
	$x1 := $_t4
	return $r2

fun if1:
entry:
	if $a0 >= 0 then goto L5 else goto L4
L4:
	$_t5 := 0 - 7
	$b1 := $_t5
	goto L5
L5:
	$b2 = fi [(entry,$b0),(L4,$b1)]
	return $b2

fun if2:
entry:
	if $a0 >= 0 then goto L7 else goto L6
L6:
	$_t6 := 0 - 7
	$b1 := $_t6
	goto L8
L7:
	$b1 := 42
	goto L8
L8:
	return $b1

fun main:
entry:
	$i1 := $n0
	$r1 := 1
	goto L2
L1:
	$_t1 := $r2 * $i2
	$_t2 := $_t1 * $i2
	$r3 := $_t1 * $i2
	$_t3 := $i2 - 1
	$i3 := $i2 - 1
	goto L2
L2:
	$i2 = fi [(entry,$n0),(L1,$i3)]
	$n1 := $n0
	$r2 = fi [(entry,1),(L1,$r3)]
	if $i2 > 0 then goto L1 else goto L3
L3:
	$_t4 := $n0 + 3
	$x1 := $n0 + 3
	return $r2

fun if1:
entry:
	if $a0 >= 0 then goto L5 else goto L4
L4:
	$_t5 := 0 - 7
	$b1 := 0 - 7
	goto L5
L5:
	$b2 = fi [(entry,$b0),(L4,$b1)]
	return $b2

fun if2:
entry:
	if $a0 >= 0 then goto L7 else goto L6
L6:
	$_t6 := 0 - 7
	$b1 := 0 - 7
	goto L8
L7:
	$b1 := 42
	goto L8
L8:
	return $b1

fun main:
entry:
	goto L2
L1:
	$_t1 := $r2 * $i2
	$r3 := $_t1 * $i2
	$i3 := $i2 - 1
	goto L2
L2:
	$i2 = fi [(entry,$n0),(L1,$i3)]
	$r2 = fi [(entry,1),(L1,$r3)]
	if $i2 > 0 then goto L1 else goto L3
L3:
	return $r2

fun if1:
entry:
	if $a0 >= 0 then goto L5 else goto L4
L4:
	$b1 := 0 - 7
	goto L5
L5:
	$b2 = fi [(entry,$b0),(L4,$b1)]
	return $b2

fun if2:
entry:
	if $a0 >= 0 then goto L7 else goto L6
L6:
	$b1 := 0 - 7
	goto L8
L7:
	$b1 := 42
	goto L8
L8:
	return $b1

