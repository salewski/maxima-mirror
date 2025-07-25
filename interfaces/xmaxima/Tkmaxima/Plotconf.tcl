############################################################
# Plotconf.tcl                                             #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
############################################################

proc makeFrame { w type } {
    # If the plot was produced by Xmaxima's console, it will be in a frame
    # inserted in the console. Otherwise it will be a toplevel window
    global doExit fontSize buttonfont
    set win $w
    if { "$w" == "." } {
        set w ""
    } else {
	catch { destroy $w}
	if { [regexp {^\.(plot|versust)} $w] } {
            tk::toplevel $w
        } else {
            ttk::frame $w
        }
    }
    # hide the parent window and destroy it when the canvas is destroyed
    set dismiss "destroy $win"
    catch { set parent [winfo parent $w]
	if { "$parent" == "." } {
	    set dismiss "destroy ."
	}
	if { [regexp {^\.(plot|versust)} [winfo toplevel $win]] } {
            set top [winfo parent $win]
            set dismiss "destroy $top"
            wm withdraw $top
	}
    }
    if { "$doExit" != "" } {set dismiss $doExit } 	
    set dismiss [concat $dismiss "; clearLocal $win "]

    # Win is the name of the window and w its name without a final period
    oset $win type $type
    set c $w.c
    oset $win c $c
    bboxToRadius $win
    set buttonFont $buttonfont
    oset $win buttonFont $buttonfont

    # widgets for the menu buttons and (x, y) coordinates label
    if { $type == {df} } {
        set ltext [mc "Click on the graph\nto trace a curve"]
    } else {
        set ltext [mc "Pointer Coordinates"]
    }
    ttk::label $w.position -text $ltext -background white -font $buttonFont
    set mb [frame $w.menubar]
    pack $mb -fill x
    ttk::button $mb.close -text [mc "Close"] -command $dismiss
    ttk::button $mb.config -text [mc "Config"] -command "doConfig$type $win"
    ttk::button $mb.save -text [mc "Save"] \
        -command "mkPrintDialog .dial -canvas $c -buttonfont $buttonFont "
    pack $mb.close $mb.config $mb.save -side left
    set buttonsLeft 1

    # c is the canvas where the plot will be drawn
    tk::canvas $c -cursor arrow -background white \
        -width [oget $win width] -height [oget $win height]
    pack $c -side top -expand 1 -fill both
    bind $c <Motion> "showPosition $w %x %y"
    bind $c <Configure> "reConfigure %W %w %h"
    
    $w.position config -background [$c cget -background]
    place $w.position -in $w.c -relx 0.03 -rely 0.99 -anchor sw
    raise $w.position
    focus $w
    addSliders $win
    if { [regexp {^\.(plot|versust)} [winfo toplevel $win]] } {
        pack [ttk::sizegrip $w.szgr] -side bottom -anchor se
        bind $win <Control-w> $dismiss
        wm protocol $w WM_DELETE_WINDOW $dismiss
    }
    return $win
}

proc mkentry { newframe textvar text buttonFont } {
    frame $newframe
    set parent $newframe
    set found 0
    while { !$found } {
	set parent [winfo parent $parent]
	if { "$parent" == "" } { break }
	if { ![catch {  set type [oget $parent type] } ] } {
	    global plot[set type]Options
	    foreach v [set plot[set type]Options] {
		if { "[oloc $parent [lindex $v 0]]" == "$textvar" } {
		    setBalloonhelp $parent $newframe [lindex $v 2]
		    set found 1
		    break}}}}
    label $newframe.lab1
    label $newframe.lab -text "$text:" -font $buttonFont -width 0
    entry $newframe.e -width 20 -textvariable $textvar -font $buttonFont
    pack $newframe.lab1 -side left -expand 1 -fill x
    pack $newframe.lab -side left
    pack $newframe.e -side right -padx 3 -fill x
    # pack $newframe.lab $newframe.e -side left -padx 3 -expand 1 -fill x}

proc pushBind { win key action } {
    pushl [bind $win $key] [list $win $key ]
    bind $win $key $action}

proc popBind { win key  } {
    set binding [popl [list $win $key] {}]
    bind $win $key $binding}

proc showPosition { win x y } {
    # global position c
    makeLocal $win c
    # we catch so that in case have no functions or data..
    catch {
	$win.position config -text \
	    "[format {(%.6g,%.6g)}  [storx$win [$c canvasx $x]] [story$win [$c canvasy $y]]]"}}

proc reConfigure { c width height  } {
    set win [winfo parent $c]
    set w [oget $win width]
    set h [oget $win height]
    set wscale [expr double($width)/$w]
    set hscale [expr double($height)/$h]
    $c scale all 0 0 $wscale $hscale
    oset $win width $width
    oset $win height $height
    set_xy_transforms $win}

proc writePostscript { win } {
    global  printOption argv
    makeLocal $win c transform transform0 xmin ymin xmax ymax
    set rtosx rtosx$win ; set rtosy rtosy$win
    drawPointsForPrint $c
    if { "[$c find withtag printrectangle]" == "" } {
	$c create rectangle [$c canvasx 0] [$c canvasy 0] \
          [$c canvasx [$c cget -width ]] [$c canvasy [$c cget -height ]] \
          -tags printrectangle -outline white}
    set bbox [eval $c bbox [$c find withtag printrectangle]]
    desetq "x1 y1 x2 y2" $bbox
    # set title "unknown plot"
    # catch { set title [eval $printOption(maintitle)] }
    # $c create text [expr {($x1 + $x2)/2}]  [expr {$y1 + .04 * ($y2 - $y1)}] \
    # 	    -anchor center -text $title -tag title
    update
    #set diag [vectorlength [expr {$y1-$x1}] [expr {$y2-$x2}]]
    #  get rid of little arrows that creep onto the outside, ie let
    #  the blank rectangle cover them.
    #set x1 [expr {$x1+.01 * $diag}]
    #set x2 [expr {$x2-.01 * $diag}]
    #set y1 [expr {$y1+.01 * $diag}]
    #set y2 [expr {$y2-.01 * $diag}]
    # Set up font replacement list
    catch {set fontMap([font create -family {BitstreamVeraSansMono} -size 10]) [list Courier 14]}
    catch {set fontMap([font create {helvetica 16 normal}]) [list Helvetica 16]}
    set com "$c postscript  \
      	    -x [expr {($x1 - 35)}] -y [expr {($y1 -25)}] \
	    -width [expr {($x2 - $x1 + 60)}] \
            -height [expr {($y2 - $y1 + 45)}] \
            -fontmap fontMap \
	    [getPageOffsets [expr {($x2 - $x1 + 55)/(1.0*($y2 - $y1 + 45))}] ]"

    #puts com=$com
    set output [eval $com]
    set fi [open $printOption(psfilename) w]
    puts $fi $output
    close $fi}

proc vectorlength {a b} {return [expr {sqrt($a*$a+$b*$b)}]}

proc setupCanvas { win } {
    makeLocal $win xcenter xradius ycenter yradius
    oset $win xmin [expr {$xcenter - $xradius}]
    oset $win xmax [expr { $xcenter + $xradius}]
    oset $win ymin [expr { $ycenter - $yradius}]
    oset $win ymax [expr { $ycenter + $yradius} ]}

#
#-----------------------------------------------------------------
#
# compose --  A and B are transformations of the form "origin scalefac"
# and composing them means applying first b then a, as in a.b.x
#  "o s" . x ==> (x-o)*s + o
#  Results: the "origin scalefac" which corresponds to the composition.
#
#----------------------------------------------------------------
#
proc compose { a b } {
    return  "[expr {-[lindex $a 1]*[lindex $b 0]*[lindex $b 1] \
      +[lindex $a 1]*[lindex $b 0]-[lindex $a 0]*[lindex $a 1] \
      +[lindex $a 0]}] [expr {[lindex $a 1]*[lindex $b 1]}]"
}

proc sparseListWithParams { form variables paramlist } {
    set tem [parseConvert $form -doall 1]
    # puts tem=$tem
    set params [splitParams $paramlist]
    if { [catch {set res [substParams [lindex $tem 0] $variables $params] }\
	      err ] } {
	set vars [lindex $tem 1]
	set all $variables
	foreach { v val }  $params { lappend all $v}
	foreach v $vars { if { [lsearch $all [string range $v 1 end]] < 0 } {
	    error [mc "The variable %s appeared in %s but was not in allowed variables: %s or in parameters: %s" "`[string range $v 1 end]'" $form $variables $paramlist] }}
	error [mc "The form %s may involve variables other than %s or the parameters %s, or the latter may have invalid expressions: %s" $form $variables $paramlist $err] }
    return $res
}

proc sparseWithParams { form variables params } {
    set tem [sparseListWithParams $form $variables $params]
    if { [llength $tem ] > 1 } { error [concat [mc "only wanted one function:"] "$form"]}
    lindex $tem 0}

#-----------------------------------------------------------------
#
# myVarSubst --  into FORM substitute where
# listVarsVals where each element of this list may mention
# the previous values eg "k 7 ll sin(k+8)"
# eg:
#myVarSubst [lindex [parseConvert "k*x+l" -doall 1] 0] {x $x k 27+4 l 93+k^3}
# ==> {((31 * $x) + 29884.0)}
#
#  Results: FORM with the substitutions done
#
#----------------------------------------------------------------
proc myVarSubst { form listVarsVals } {
    foreach {_u _v} $listVarsVals {
	if { "\$$_u" == "$_v" } {
	    set $_u $_v
	} else {
	    set _f1 [lindex [parseConvert  $_v -doall 1] 0]
	    set $_u [expr [lindex $_f1 0]]
	    # puts "$_u = [set $_u]"
	}}
    subst -nobackslashes -nocommands $form}

proc splitParams { paramlist } {
    set params ""
    foreach v [split $paramlist ,] {
	set tem [split $v =]
	if { [llength $tem] == 2 } {
	    lappend params [lindex $tem 0] [lindex $tem 1]}}
    return $params}

#-----------------------------------------------------------------
#
# substParams --  substitute into FORM keeping VARIABLES as they are
# and the PARAMLIST (of the form k=23, l=k+7,...) into FORM
#
#  Results: substituted FORM
#
#----------------------------------------------------------------
proc substParams { form variables params } {
    foreach v $variables { lappend params $v \$$v}
    set res [myVarSubst $form $params]
    return $res}

#-----------------------------------------------------------------
#
# set_xy_region --  set up the bounds of the x and y coordinates
# that will appear on the plot and the part of the window that will
# be filled by the plot (fac, a number between 0 and 1).
#
#----------------------------------------------------------------
proc set_xy_region { win fac } {
    makeLocal $win xcenter ycenter xradius yradius c
    set xmin [expr {$xcenter - $xradius}]
    set xmax [expr {$xcenter + $xradius}]
    set ymin [expr {$ycenter - $yradius}]
    set ymax [expr {$ycenter + $yradius}]
    oset $win fac $fac
    oset $win xmin $xmin
    oset $win xmax $xmax
    oset $win ymin $ymin
    oset $win ymax $ymax}

#-----------------------------------------------------------------
#
# set_xy_transforms --  set up transformations for the canvas of WINDOW
# so that the plot is a fraction of the window (fac).
# these transformations are used to convert from real values of x and y
# to screen coordinates in pixels and vice versa.
#
#  Side Effects: transform functions rtosx$win rtosy$win storx$win story$win
#  are defined.
#
# $rtosx,$rtosy --  convert Real coordinate to screen coordinate
# $storx,$story --  Convert a screen coordinate to a Real coordinate.
#
#----------------------------------------------------------------
proc set_xy_transforms { win } {
    makeLocal $win xmin ymin xmax ymax width height fac
    if { [oget $win type] == {3d} } {
        set f1 [expr {(1 - $fac)/2.0}]
        set x1 [expr {$f1 *$width}]
    } else {
        set f1 [expr {(1 - $fac)/3.0}]
        set x1 [expr {2* $f1 *$width}]}
    set y1 [expr {$f1 *$height}]
    set x2 [expr {$x1 + $fac*$width}]
    set y2 [expr {$y1 + $fac*$height}]
    # Do not use the extra vertical space for sliders
    linkLocal $win sliders
    if {[string length $sliders] > 0} {
        set y2 [expr {$y2 - 40*[llength [split $sliders ,]]}]}

    set transform [makeTransform "$xmin $ymin $x1 $y2" "$xmin $ymax $x1 $y1 " \
                       "$xmax $ymin $x2 $y2"]
    oset $win transform $transform
    oset $win transform0 $transform
    getXtransYtrans $transform rtosx$win rtosy$win
    getXtransYtrans [inverseTransform $transform] storx$win story$win}
                        
proc inputParse { in } {
    if { [regexp -indices \
	      {D\[([a-zA-Z][0-9a-zA-Z]*[ ]*),([a-zA-Z][0-9a-zA-Z]*[ ]*)\] *=} \
	      $in all1 i1 i2] } {
	set v1 [getOneMatch $in $i1]
	set v2 [getOneMatch $in $i2]
	set s1 [string range $in [lindex $all1 1] end]

	if { [regexp -indices {,[ \n]*D\[([a-zA-Z][0-9a-zA-Z]*[ ]*),([a-zA-Z][0-9a-zA-Z]*[ ]*)\] *=} \
		  $s1 all2 i1 i2] } {
	    set v3  [getOneMatch $s1 $i1]
	    set v4 [getOneMatch $s1 $i2]
	    set end [string first \} $s1 ]
	    set form2 [string range $s1 [expr {1 + [lindex $all2 1]}] [expr {$end -1}]]
	    if { "$v4" != "$v2" } {error [concat [mc "different variables"] "$v2" [mc "and"] "$v4"]}

	    set form1 [string range $in [expr {1 + [lindex $all1 1]}] [expr {[lindex $all2 0] + -1 + [lindex $all1 1]}]]
	    # puts "v1=$v1,form1=$form1,form2=$form2"
	    return [list  $v2 $v1 $v3 $form1 $form2]}}}

proc composeTransform { t1 t2  } {
    desetq "a11 a12 a21 a22 e1 e2" $t1
    desetq "b11 b12 b21 b22 f1 f2" $t2
    return  [list \
		 [expr {$a11*$b11+$a12*$b21}] \
		 [expr {$a11*$b12+$a12*$b22}] \
		 [expr {$a21*$b11+$a22*$b21}] \
		 [expr {$a22*$b22+$a21*$b12}] \
		 [expr {$a11*$f1+$a12*$f2+$e1}] \
		 [expr {$a21*$f1+$a22*$f2+$e2}]]}

#-----------------------------------------------------------------
#
# makeTransform --  Given three points mapped to three other points
# write down the affine transformation (A.X+B) which performs this.
# the arguments are of the form "x1 y1 u1 v1" "x2 y2 u2 v2" "x3 y3 u3 v3"
# where (x1,y1) --> (u1,v1)  etc.
#  Results: an affine transformation "a b c d e f" which is
#     [ a  b ]  [ x1 ] + [ e ]
#     [ c  d ]  [ y1 ]   [ f ]
#  Side Effects: none
#
#----------------------------------------------------------------
proc makeTransform { P1 P2 P3 } {
    desetq  {X1 Y1 U1 V1} $P1
    desetq  {X2 Y2 U2 V2} $P2
    desetq  {X3 Y3 U3 V3} $P3
    set tem [expr {double((($X2-$X1)*$Y3+($X1-$X3)*$Y2+($X3-$X2)*$Y1))}]
    set A [expr {(($U2-$U1)*$Y3+($U1-$U3)*$Y2+($U3-$U2)*$Y1) \
		     /$tem}]
    set B [expr {-(($U2-$U1)*$X3+($U1-$U3)*$X2+($U3-$U2)*$X1) \
		     /$tem}]
    set E [expr {(($U1*$X2-$U2*$X1)*$Y3+($U3*$X1-$U1*$X3)*$Y2+($U2*$X3-$U3*$X2)*$Y1) \
		     /$tem}]
    set C [expr {(($V2-$V1)*$Y3+($V1-$V3)*$Y2+($V3-$V2)*$Y1) \
		     /$tem}]
    set D [expr {-(($V2-$V1)*$X3+($V1-$V3)*$X2+($V3-$V2)*$X1) \
		     /$tem}]
    set F [expr {(($V1*$X2-$V2*$X1)*$Y3+($V3*$X1-$V1*$X3)*$Y2+($V2*$X3-$V3*$X2)*$Y1) \
		     /$tem}]
    return [list $A $B $C $D $E $F]}

#-----------------------------------------------------------------
#
# getXtransYtrans --   If the x coordinate transforms independently
#  of the y and vice versa, give expressions suitable for building a
# proc.
#
#----------------------------------------------------------------
#
proc getXtransYtrans { transform p1 p2 } {
    desetq "a b c d e f"  $transform
    if { $b == 0  && $c == 0 } {
	proc $p1 { x } "return \[expr {$a*\$x+$e}\]"
	proc $p2 { y } "return \[expr {$d*\$y+$f} \]"
	return 1}
    return 0}

#-----------------------------------------------------------------
#
# inverseTransform --   Find the inverse of an affine transformation.
#
#----------------------------------------------------------------
proc inverseTransform { transform } {
    desetq "a b c d e f" $transform
    set det [expr {double($a*$d - $b*$c)}]
    return [list [expr {$d/$det}] [expr {- $b / $det }] [expr {- $c / $det}] \
                [expr {$a / $det}]  [expr {($b*$f-$d*$e)/ $det }] \
                [expr {-($a*$f-$c*$e)/ $det}]]}

#-----------------------------------------------------------------
#
# getTicks --  given an interval (a,b) subdivide it and
# calculate where to put the ticks and what to print there.
# we want DESIRED number of ticks, but we also want the ticks
# to be at points in the real coords of the form .2*10^i or .5*10^j
#  Results: the ticks
#
#----------------------------------------------------------------
proc getTicks { a b n } {
    set len [expr {(($b - $a))}]
    if { $len < [expr {pow(10,-40)}] } { return ""}
    set best 0
    foreach v { .1 .2 .5 } {
	# want $len/(.1*10^i) == $n
	set val($v)  [expr {ceil(log10($len/(double($n)*$v)))}]
	set use [expr {$v*pow(10,$val($v))}]
	set fac [expr {1/$use}]
	set aa [expr {$a * $fac}]
	set bb [expr {$b * $fac}]
	set j [expr {round(ceil($aa)) }]
	set upto [expr {floor($bb) }]
	if { $upto-$j > 14} {
	    set step 5
	} else {
	    set step 2
	}
	set ticks ""
	while { $j <= $upto } {
	    set tt [expr {$j / $fac}]
	    if { $j%$step == 0 } {
		append ticks " { $tt $tt }"
	    } else  {append ticks " $tt"}
	    incr j}
	set answer($v) $ticks
	set this [llength $ticks]
	if { $this  > $best } {
	    set best $this
	    set at $v}
	#puts "for $v [llength $ticks] ticks"
    }
    #puts "using $at [llength $answer($at)]"
    return $answer($at)}

proc axisTicks { win c }  {
    $c delete axisTicks
    if { ![catch {oget $win noaxisticks}] } { return }
    set swid [$c cget -width]
    set shei [$c cget -height]
    set x1 [storx$win [$c canvasx 0]]
    set y1 [story$win [$c canvasy 0]]
    set x2 [storx$win [$c canvasx $swid]]
    set y2 [story$win [$c canvasy $shei]]
    #puts "x1=$x1,y1=$y1,y2=$y2,x2=$x2"
    if { $y1 > 0  &&  $y2 < 0 } {
	set ticks [getTicks $x1 $x2 [expr {$swid/50}] ]
	#puts "ticks=$ticks"
	set eps [expr {.005 * abs($y1 - $y2)}]
	set neps [expr {-.005 * abs($y1 - $y2)}]
	set donext 0
	foreach v $ticks {
	    set x [lindex $v 0]
	    set text [lindex $v 1]
	    if { $donext } {set text [lindex $v 0] ; set donext 0 }
	    if { [lindex $v 0] == 0 } { set text "" ; set donext 1 }
	    #puts " drawTick $c $x 0 0 $neps 0 $eps  $text axisTicks"
	    drawTick $c $x 0 0 $neps 0 $eps  $text axisTicks}}
    if { 0 < $x2 && 0 > $x1 } {
	set ticks [getTicks $y2 $y1 [expr {$shei/50}]]
	set eps [expr {.005 * ($x2 - $x1)}]
	set neps [expr {-.005 * ($x2 - $x1)}]
	set donext 0
	foreach v $ticks {
	    set y [lindex $v 0]
	    set text [lindex $v 1]
	    if { $donext } {set text [lindex $v 0] ; set donext 0}
	    if { [lindex $v 0] == 0 } { set text "" ; set donext 1}
	    drawTick $c 0 $y $neps 0 $eps 0  $text axisTicks}}}

#-----------------------------------------------------------------
#
# marginTicks --  draw ticks around the border of window
#  x1,y1  top left x2,y2 bottom right.
#
#----------------------------------------------------------------
proc marginTicks { c x1 y1 x2 y2 tag }  {
    global printOption
    set win [winfo parent $c]

    if { ![catch {oget $win noaxisticks}] } { return }
    $c delete marginTicks
    set ticks [getTicks $x1 $x2 $printOption(xticks)]
    # puts "x=$x1 $x2"
    set eps [expr {.008 * ($y1 - $y2)}]
    set neps [expr {-.008 * ($y1 - $y2)}]
    foreach v $ticks {
	set x [lindex $v 0]
	set text [lindex $v 1]
	drawTick $c $x $y1 0 0 0 $eps $text $tag
	drawTick $c $x $y2 0 0 0 $neps {} $tag}
    #puts "y=$y2,$y1"
    set ticks [getTicks $y1 $y2 $printOption(yticks)]
    set eps [expr {.005 * ($x2 - $x1)}]
    set neps [expr {-.005 * ($x2 - $x1)}]
    set donext 0
    foreach v $ticks {
	set y [lindex $v 0]
	set text [lindex $v 1]
	drawTick $c $x1 $y 0 0 $neps 0 $text $tag
	drawTick $c $x2 $y 0 0 $eps 0 {} $tag}}

proc drawTick {c x y dx dy ex ey n tags} {
    global axisGray     fontCourier8
    set win [winfo parent $c]
    set rtosx rtosx$win ; set rtosy rtosy$win
    set it [$c create line [$rtosx [expr {$x +$dx}]] [$rtosy [expr {$y +$dy}]] [$rtosx [expr {$x +$ex}]] [$rtosy [expr {$y +$ey}]] -fill $axisGray -tags $tags]
    $c lower $it
    if { "$n" != "" } {
	if { $ey > 0 } { set anch s
	} elseif { $ex > 0 } {set anch w
	} elseif { $ex < 0 } {set anch e
	} elseif { $ey < 0 } {set anch n}
	$c create text  [$rtosx [expr {$x +1.5*$ex}]] [$rtosy [expr {$y +1.5*$ey}]] \
	    -text [format "%.8g" $n] -font $fontCourier8 -tags $tags \
	    -anchor $anch}}

proc doConfig { win }  {
    makeLocal $win c buttonFont
    $c delete configoptions
    set canv $c
    # set w $c.config
    set w $win.config
    catch {destroy $w}
    frame $w -borderwidth 2 -relief raised

    label $w.msg  -wraplength 600 -justify left -text [mc "Plot Setup"] -font $buttonFont
    pack $w
    pack $w.msg -side top
    set wb1 $w.choose1
    frame $wb1
    set wb2 $w.choose2
    frame $wb2
    pack $wb1 $wb2 -side left -fill x -pady 2m
    set item [$canv create window [$canv canvasx 10] [$canv canvasy  10] -window $w -anchor nw -tags configoptions]
    button $wb1.dismiss -command  "$canv delete $item; destroy $w " -text "ok" -font $buttonFont
#    button $wb1.printoptions -text [mc "Print Options"] -command "mkPrintDialog .dial -canvas $c -buttonfont $buttonFont " -font $buttonFont

    pack $wb1.dismiss -side top
    return "$wb1 $wb2"}
# mkentry { newframe textvar text }

# turn off the horrible show_balloons by default.
global show_balloons
set show_balloons 0

proc balloonhelp { win subwin msg } {
    global show_balloons

    if { $show_balloons == 0 } {return}
    linkLocal  [oget $win c] helpPending
    if { [info exists helpPending] } {after cancel $helpPending}
    set helpPending [after 1000 [list balloonhelp1 $win $subwin $msg]]
}

proc balloonhelp1 { win subwin msg } {
    if { ![winfo exists $win] } { return }
    makeLocal $win c buttonFont
    set x0 [winfo rootx $win]
    set y0 [winfo rooty $win]
    set atx [expr {[winfo rootx $subwin] + [winfo width $subwin] - $x0} ]
    set aty [expr {[winfo rooty $subwin] + [winfo height $subwin] - $y0} ]
    set wid [$c cget -width]
    set wid2 [expr {round ($wid /2.0)}]
    set wid10 [expr {round ($wid /10.0)}]
    if { $aty <=1 } { set aty 30 }
    incr aty 10
    incr atx 10
    set atx [$c canvasx $atx]
    set aty [$c canvasy $aty]
    #puts "$atx $aty"
    $c delete balloon
    $c create text $atx $aty -anchor nw -text $msg -font $buttonFont -width $wid2 -fill white -fill black -tags "balloon btext"
    desetq "x1 y1 x2 y2" [$c bbox btext]
    set x1 [expr {$x1 - .3*($x2-$x1)}]
    set x2 [expr {$x2 + .3*($x2-$x1)}]
    set y1 [expr {$y1 - .3*($y2-$y1)}]
    set y2 [expr {$y2 + .3*($y2-$y1)}]

    eval $c create polygon $x1 $y1  $x2 $y1 $x2 $y2 $x1 $y2  -fill beige -tags balloon -smooth 1
    $c raise btext}

proc setBalloonhelp { win subwin msg } {
    makeLocal $win c
    bind $subwin <Enter> "balloonhelp $win $subwin [list $msg]"
    bind $subwin <Leave> "deleteBalloon $c"
}

proc deleteBalloon { c } {
    linkLocal $c helpPending
    if { [info exists helpPending] } {
	after cancel $helpPending
	unset helpPending}
    $c delete balloon}

#-----------------------------------------------------------------
#
# minMax --  Compute the max and min of the arguments, which may
# be vectors or numbers
#
#----------------------------------------------------------------
proc minMax { args } {
    set max [lindex [lindex $args 0] 0] ; set min $max ;
    foreach vec $args {
	foreach v $vec {
	    if { $v > $max } {set max $v }}}
    return [list $min $max]}

proc matrixMinMax { list } {
    # compute the min max of the list
    set min +10e300
    set max -10e300
    foreach mat $list {
	foreach row $mat {
	    foreach v [ldelete nam $row] {
		if { $v > $max } {catch  { set max [expr {$v + 0}] }}
		if { $v < $min} {catch  { set min [expr {$v + 0}] }}}}}
    list $min $max}

proc omPlotAny { data args } {
    # puts "data=<[lindex $data 0]>"
    set command [list [lindex [lindex $data 0] 0]  -data [lindex $data 0] ]
    if { "[lindex $command 0]" == "plot2d" } {
	lappend command -xfun {}}
    foreach v $args { lappend command $v }
    eval $command
    #eval [lindex [lindex $data 0] 0] -xfun [list {}] -data [list [lindex $data 0]] $args
}

proc resizeSubPlotWindows { win wid height } {
    set at [$win yview "@0,0"]
    foreach w [winfo children $win] {
	if { [string match plot* [lindex [split $w .] end]] } {
	    resizePlotWindow $w [winfo width $w] $height}}
    if { "$at" != "" } { $win yview $at}}

proc resizePlotWindow  { w width height } {
    if { [winfo width $w.c] <= 1 } {
	after 100 update ;
	return }
    if { ![catch { set tem [oget $w lastResize] } ] && [expr {[clock seconds] - $tem }] < 2 } { return
    } else {
	oset $w lastResize [clock seconds ]
    }
    #puts "resizePlotWindow $w $width $height"
    # return
    set par [winfo parent $w]
    set facx 1.0
    set facy 1.0
    set wid [winfo width $par]
    set hei [winfo height $par]
    if { "[winfo class $par]" == "Text" } {
	set dif 10
	set wid1 $wid ; set hei1 $hei
	#puts "now w=$w"
	#set wid1 [getPercentDim [oget $w widthDesired] width $par]
	catch {set wid1 [getPercentDim [oget $w widthDesired] width $par] }
	catch {set hei1 [getPercentDim [oget $w heightDesired] height $par] }
	set wid [expr {($wid1 > $wid - 30 ? $wid - 30 : $wid1 )}]
	set hei [expr {($hei1 > $hei - 30 ? $hei - 30 : $hei1 )}]
    } else {set dif 10}
    #     if { $width > $wid -20 || $wid > $width -20 }
    if { (abs($width-$wid) > $dif ||  abs($height-$hei) > $dif)
	 &&  [winfo width $w.c] > 1 } {
	set eps [expr {2 * [$w.c cget -insertborderwidth] + [$w.c cget -borderwidth] }]
	set epsx $eps
	set epsy $eps
	set extrawidth [expr {([winfo width $w] - [winfo width  $w.c]) +$epsx}]
	set extraheight [expr {([winfo height $w] - [winfo height  $w.c]) +$epsy}]
	set nwidth [expr {$wid - ($extrawidth > 0  ? $extrawidth : 0)}]
	set nheight [expr {$hei - ($extraheight > 0  ? $extraheight : 0)}]

	#puts "$w.c config -width $nwidth  -height $nheight, extraheight=$extraheight,epsy=$epsy"
	$w.c config -width $nwidth  -height $nheight}}

proc bboxToRadius { win  } {
    makeLocal $win bbox
    if { "$bbox" != "" } {
	linkLocal $win xradius yradius xcenter ycenter
	set i 0
	foreach v { x y z } {
	    set min [lindex $bbox $i]
	    set max [lindex $bbox [expr {$i+2}]]
	    if { "$min" != "" } {
		if { $min >= $max } {error "bad bbox $bbox since $min >= $max"}
		set ${v}radius [expr { ($max - $min) /2.0}]
		set ${v}center [expr { ($max + $min) /2.0}]}}}}

proc updateParameters { win var value} {
    linkLocal $win parameters
    # puts "$win $var $value"
    set ans ""
    set comma ""
    foreach {v val} [splitParams $parameters] {
        if { "$v" == "$var" } {set val $value}
	append ans $comma $v=$val
	set comma ","}
    #    puts "parameters=$ans"
    set parameters $ans}

proc addSliders { win } {
    linkLocal $win sliders c width parameters
    set i 0
    if { "$sliders" == "" } { return }
    catch { destroy $c.sliders }
    set bg "#9ce"
    set trough "#9df"
    frame $c.sliders -relief raised -highlightthickness 2 -highlightbackground $trough
    foreach v [split $sliders ,] {
	if { [regexp {([a-zA-Z0-9]+)[ ]*=?(([---0-9.]+):([---0-9.]+))?} $v  junk var junk x0 x1] } {
	    incr i
	    if { "$x0" == "" } { set x0 -5  ; set x1 5}
	    set fr $c.sliders.fr$i
	    frame $fr -background $bg
	    label $fr.lab -text $var: -background $bg
	    label $fr.labvalue -textvariable [oloc $win slidevalue$i]  -background $bg -relief sunken -justify left
	    scale $fr.scale -command "sliderUpdate $win $var" \
		-from "$x0" -to $x1 -orient horizontal \
		-resolution [expr ($x1 - $x0) < 1 ? ($x1-$x0)/100.0 : .01] \
		-length [expr {$width/2}] -showvalue 0 -variable \
                [oloc $win slidevalue$i] -background $bg -troughcolor "#9ad" \
                -highlightthickness 0
	    pack $fr.lab -side left -expand 1 -fill x
	    pack $fr.labvalue $fr.scale -side left
	    pack  $fr -side top -expand 1 -fill x
	    set found 0
	    set val  [assoc $var [splitParams $parameters] no]
	    if { "$val" == "no" } {
		set val  [expr ($x1 + $x0)/2.0]
		if { "$parameters" != "" }  { append parameters , }
		append parameters $var=$val}
	    $fr.scale set $val}}
    place $c.sliders -in $c -relx 1.0 -x -4 -rely 1.0 -y -4 -anchor se}

proc sliderUpdate { win var val } {
    linkLocal $win sliderCommand parameters
    set params $parameters
    updateParameters $win $var $val
    if {"$params" ne "$parameters" && [info exists sliderCommand] } {
        $sliderCommand $win $var $val}}

## endsource plotconf.tcl
