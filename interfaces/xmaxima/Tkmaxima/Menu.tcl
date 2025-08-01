############################################################
# Menu.tcl                                                 #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
############################################################

proc zoomConsole {f} {
    set ffamily [lindex $::xmaxima_default(ConsoleFont) 0]
    set fsize [lindex $::xmaxima_default(ConsoleFont) 1]
    set fsize [expr round($fsize*pow(1.2,$f))]
    font configure ConsoleFont -family $ffamily -size $fsize
    set text $::xmaxima_priv(cConsoleText)
    resizeSubPlotWindows $text [winfo width $text] [winfo height $text]
    resizeMaxima $text [winfo width $text] [winfo height $text]
    set ::xmaxima_default(ConsoleFont) [list $ffamily $fsize]
}

proc fileType {type} {
    switch -exact -- $type output {
        return [list {"Output Files" .out} {"All Files" *}]
    } lisp {
        return [list {"Lisp Files" {.lisp}} {"All Files" *}]
    } maxima {
        return [list {"Maxima Files" {.mac .mc .dem}} {"All Files" *}]
    } all - default {
        return [list {"All Files" *}]}}

proc getOpenFile {title {type "all"}} {
    set types [fileType $type]
    set file $::xmaxima_default(OpenFile)
    if {$file ne ""} {set dir [file dir $file]} {set dir ""}
    set proc tk_getOpenFile
    set list [list $proc -title $title -filetypes $types]
    if {$dir ne ""} {lappend list  -initialdir [file native $dir]}
    if {[catch {eval $list} retval]} {
	global errorInfo
	tk_messageBox -title Error -icon error -message \
            [mc "Error opening file:\n%s" $errorInfo]
	return ""}
    if {$retval ne ""} { set ::xmaxima_default(OpenFile) $retval }
    return $retval}

proc getSaveFile {title {type "all"}} {
    set types [fileType $type]
    set file $::xmaxima_default(SaveFile)
    if {$file ne ""} {set dir [file dir $file]} {set dir ""}
    set proc tk_getSaveFile
    set list [list $proc -title $title -filetypes $types]
    if {$dir ne ""} {lappend list -initialdir [file native $dir]}
    if {[catch {eval $list} retval]} {
	global errorInfo
	tk_messageBox -title Error -icon error -message \
            [mc "Error saving file:\n%s" $errorInfo]
	return ""}
    if {$retval ne ""} { set ::xmaxima_default(SaveFile) $retval }
    return $retval}

proc cIDECreateEvent {text label code} {
    set z [winfo toplevel $text]
    set event "<<[join $label -]>>"
    bind $text $event $code
    return [list event generate $text $event]
}

proc pMAXSaveTexToFile {text} {
    set file [getSaveFile [mc "Save to a file"] output]
    if {$file != ""} {
	set contents [$text get 1.0 end]
	set fd [open $file w]
	if {[catch {puts $fd $contents} err]} {
	    tk_messageBox -title Error -icon error -message \
                [mc "Error writing to file:\n%s" $err]
	}
	catch {close $fd}
    }
}

proc vMAXAddBrowserMenu {win} {
    if {[winfo exists .browser.menu]} {destroy .browser.menu}
    set bm .browser.menu
    menu $bm
    .browser configure -menu .browser.menu
    foreach m {file edit options help} {
        $bm add cascade -label [string totitle $m] -underline 0 \
            -menu [menu $bm.$m -tearoff 0]
    }
    $bm.file add command -label [mc Reload] -underline 0 \
	-command "OpenMathOpenUrl \[oget \[omPanel $win\] location\] -reload 1 \
		      -commandpanel \[omPanel $win\]"
    $bm.file add command -label [mc Interrupt] -underline 0 \
        -command "omDoInterrupt \[oget \[omPanel $win\] textwin\]"
    $bm.file add command -label [mc Abort] -underline 0 \
	-command "omDoAbort \[oget \[omPanel $win\] textwin\]"
    $bm.file add command -label [mc Stop] -underline 1 \
	-command "omDoStop \[oget \[omPanel $win\] textwin\]"
    $bm.file add command -label [mc Forget] -underline 0 \
        -command  "forgetCurrent \[omPanel $win\]"
    $bm.file add command -label [mc Save] -underline 0\
	-command "pMAXSaveTexToFile \[oget \[omPanel $win\] textwin\]"
    $bm.file add separator
    $bm.file add command -label [mc {Close}] -underline 0 \
        -command "destroy $win"
    $bm.options add command -label [mc {Fonts}] -underline 0 \
        -command {fontDialog .fontdialog}
}

proc vMAXAddSystemMenu {fr text} {
    set win $fr.textcommands
    # Build a menubar
    if {[winfo exists .menu]} {destroy .menu}
    menu .menu
    . configure -menu .menu
    # Add a File menu
    set m [menu .menu.file -tearoff 0]
    .menu add cascade -label [mc "File"] -menu $m -underline 0
    $m add command -underline 0 \
	-accel {Alt+b} \
	-label [set label [mc "Batch File"]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [getOpenFile [mc "Open a file to Batch"] maxima]
	    if {$file != ""} {
		sendMaxima $::xmaxima_priv(cConsoleText) "batch(\"$file\")\$\n"
		maxStatus [concat [mc "Batched File "] "$file"]
	    }
	}]]
    bind $text <Alt-Key-b> $command
    $m add command -underline 11 \
	-accel {Alt+o} \
	-label [set label [mc "Batch File Silently"]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [getOpenFile [mc "Open a file to Batch Silently"] maxima]
	    if {$file != ""} {
		sendMaxima $::xmaxima_priv(cConsoleText) "batchload(\"$file\")\$\n"
		maxStatus [concat [mc "Batched File "] "$file"]
	    }
	}]]
    bind $text <Alt-Key-o> $command

    $m add command -underline 11 \
	-accel {Alt+i} \
	-label [set label [mc "Restore Maxima State"]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [getOpenFile [mc "Open a file to Restore State"] lisp]
	    if {$file != ""} {
		sendMaxima $::xmaxima_priv(cConsoleText) ":lisp-quiet (prog2 (mfuncall '\$load \"$file\") nil)\n"
		maxStatus [concat [mc "Maxima State Restored from "] "$file"]
	    }
	}]]
    bind $text <Alt-Key-i> $command

    $m add separator
    $m add command -underline 0 \
	-label [set label [mc "Save Maxima State to File"]] \
	-accel {Ctrl+s} \
	-command [set command [cIDECreateEvent $text $label {
	    set file [getSaveFile [mc "Save to a file"] lisp]
	    if {$file != ""} {
		sendMaxima $::xmaxima_priv(cConsoleText) ":lisp-quiet (prog2 (mfuncall '\$save \"$file\" '\$all) nil)\n"
		maxStatus [concat [mc "Maxima State Saved to "] "$file"]
	    }
	}]]
    bind $text <Control-Key-s> $command

    $m add command -underline 1 \
	-label [set label [mc "Save Maxima Input to File"]] \
	-accel {Ctrl+a} \
	-command [set command [cIDECreateEvent $text $label {
	    set file [getSaveFile [mc "Save to a file"] maxima]
	    if {$file != ""} {
		sendMaxima $::xmaxima_priv(cConsoleText) ":lisp-quiet (prog2 (mfuncall '\$stringout \"$file\" '\$input) nil)\n"
		maxStatus [concat [mc "Maxima Input Saved to "] "$file"]
	    }
	}]]
    bind $text <Control-Key-a> $command
    $m add sep
    $m add command -underline 0 \
	-label [mc "Interrupt"] \
	-accel {Ctrl+g} \
	-command [list event generate $text <Control-Key-g>]
    $m add command -underline 0 \
	-label [mc "Restart"] \
	-command [list runOneMaxima $text]
    $m add command -underline 0 \
	-label [mc "Input prompt"] \
	-accel {Alt+s} \
	-command [list event generate $text <Alt-Key-s>]

    $m add separator
    $m add command -underline 1 -label [mc "Exit"] \
        -command [list maxExit $text]

    # Add an Edit menubutton
    set m [menu .menu.edit -tearoff 0]
    .menu add cascade -label [mc "Edit"] -menu $m -underline 0
    $m add command -underline 2 \
	-label [mc "Cut"] \
	-accel {Ctrl+x} \
	-command [list event generate $text <Control-Key-x>]
    $m add command -underline 0 \
	-label [mc "Copy"] \
	-accel {Ctrl+c} \
	-command [list event generate $text <Control-Key-c>]
    $m add command -underline 0 \
	-label [mc "Paste"] \
	-accel {Ctrl+v} \
	-command [list event generate $text <Control-Key-v>]
    #mike distinguish from Cut/Copy/Past and Kill/Yank
    $m add separator
    $m add command -underline 0 \
	-label [mc "Kill"] \
	-accel {Ctrl+k} \
	-command [list event generate $text <Control-Key-k>]
    $m add command -underline 0 \
	-label [mc "Yank"] \
	-accel {Ctrl+y} \
	-command [list event generate $text <Control-Key-y>]
    $m add separator
    #mike FIXME: use event generate
    $m add command -underline 0 \
	-label [mc "Previous Input"] \
	-accel {Alt-p} \
	-command [list CNpreviousInput $text -1]
    $m add command -underline 0 \
	-label [mc "Next Input"] \
	-accel {Alt-n} \
	-command [list CNpreviousInput $text 1]
    $m add command -underline 9 -label [mc "Clear input"] \
	-accel {Ctrl+u} \
	-command [list CNclearinput $text]
    $m add separator
    $m add command -underline 0 -label [mc "Save Console to File"] \
	-command [list pMAXSaveTexToFile $::xmaxima_priv(cConsoleText)]

    # Add an Options menubutton
    set m [menu .menu.options -tearoff 0]
    .menu add cascade -label [mc "Options"] -menu $m -underline 0
    $m add separator
    $m add command -label [mc "Open the Browser"] \
        -command [list createBrowser .browser]
    set pm [menu $m.plot]
    $m add cascade -label [mc "Plot Windows"] -menu $pm
    foreach elt { embedded separate multiple } {
	$pm add radio -label [mc [string totit $elt]] \
	    -variable ::xmaxima_default(plotwindow) \
	    -value $elt -command [list SetPlotFormat $text ]
    }
    # $m add separator
    # $m add command -underline 0 \
    #     -label [mc "Preferences"] \
    #     -command {fontDialog .preferences}
    if {[info commands console] == "console" } {
	$m add sep
	$m add command -underline 0 -label [mc "Show Tcl Console"] \
	    -command [list console show]
    }

    # Add a Maxima menubutton
    set m [menu .menu.maxima -tearoff 0]
    .menu add cascade -label "Maxima" -menu $m -underline 0
    set km [menu $m.kill]
    $m add cascade -label [mc "Clear Memory"] -menu $km
    $km add command -label [mc "Kill All"] \
	-command [list sendMaxima $text "kill(all)\$\n"]
    $km add separator
    foreach elt {labels values functions macros arrays \
		     myoptions props aliases rules gradefs \
		     dependencies let_rule_packages} {
	$km add command -label [mc [string totit $elt]] \
	    -command [list sendMaxima $text "kill($elt)\$\n"]
    }
    $m add separator  
    set dir $::xmaxima_priv(pTestsDir)  
    if {[file isdir $dir]} {
	set state normal
    } else {
	set state disabled
    }
    $m add command -underline 0 \
	-state $state \
	-label [mc "Run Tests"] \
	-command [list sendMaxima $text "run_testsuite()\$\n"]
    $m add command -underline 0 \
	-state $state \
	-label [mc "About Maxima"] \
	-command [list sendMaxima $text "build_info();\n"]

    # Add a Help menubutton
    set m [menu .menu.help -tearoff 0]
    .menu add cascade -label [mc "Help"] -menu $m -underline 0

    # Xmaxima manual
    set xmaximahelpfile [file join $::xmaxima_priv(maxima_verpkgdatadir) xmaxima html index.html]
    set xmaximahelpfile [file normalize $xmaximahelpfile]
    set xstate normal
    # if {[file isfile $xmaximahelpfile]} {
    #     set xstate normal
    # } else {
    #     set xstate disabled
    # }
    # Maxima manual
    set file $::xmaxima_priv(pReferenceToc)
    if {$::tcl_platform(platform) == "windows"} {
        # decodeURL is broken and needs fixing
        # This is a workaround
        set file [file attrib $file -shortname]
    }
    set state normal
    # if {[file isfile $file]} {
    #     set state normal
    # } else {
    #     set state disabled
    # }
    if {$::tcl_platform(platform) == "windows"} {
        if {[string match *maxima.chm* $file]} {
            $m add command -underline 1 -label [mc "Maxima Manual"] \
         	    -state $state \
	            -command [list exec hh.exe $file & ]
        } else {
            $m add command -underline 1 -label [mc "Maxima Manual (xmaxima browser)"] \
        	    -state normal \
	            -command "OpenMathOpenUrl \"file:/$file\""
        }
        $m add command -underline 4 -label [mc "Xmaxima Manual (xmaxima browser)"] \
        	-state $xstate \
	        -command "OpenMathOpenUrl \"file:/$xmaximahelpfile\""
    } else {
        $m add command -underline 1 -label [mc "Maxima Manual (xmaxima browser)"] \
        	-state $state \
	        -command "OpenMathOpenUrl \"file:/$file\""
        $m add command -underline 4 -label [mc "Xmaxima Manual (xmaxima browser)"] \
        	-state $xstate \
	        -command "OpenMathOpenUrl \"file:/$xmaximahelpfile\""
    }
    set browse {exec}

    # FIXME: get a browser object
    if {$::tcl_platform(platform) == "windows"} {
	if {$::tcl_platform(os) == "Windows 95"} {
	    # Windows 95/98/ME
	    lappend browse start
	} else {
	    # Windows NT / 2000
	    lappend browse cmd.exe /c start
	}
    } else {
	set selectedbrowser xdg-open
	foreach b { xdg-open htmlview firefox chromium-browser google-chrome konqueror epiphany galeon amaya opera } {
	    if { ! [catch {exec which $b} ] } {
		set selectedbrowser $b
		break
            }
        }
	lappend browse $selectedbrowser
    }
    $m add separator
    if {$::tcl_platform(platform) != "windows"} {
	$m add command -underline 0 -label [mc "Maxima Manual (web browser)"] \
	    -command [list eval $browse "file://$file" &]
    }
    $m add command -underline 0 -label [mc "Xmaxima Manual (web browser)"] \
	-command [list eval $browse "file://$xmaximahelpfile" &]
    $m add separator
    $m add command -underline 7 -label [mc "Maxima Homepage"] \
	-command [list eval $browse http://maxima.sourceforge.net &]
    $m add command -underline 0 -label [mc "Project Page"] \
	-command [list eval $browse https://sourceforge.net/projects/maxima/ &]
    $m add command -underline 0 -label [mc "Bug Reports"] \
	-command [list eval $browse https://sourceforge.net/p/maxima/bugs/ &]
    rename vMAXAddSystemMenu ""
    # vMAXSystemMenuHandlers $text $event
    # Backwards compatibility
    return $win
}

proc vMAXAddSystemBar {} {
    set tb [frame .toolbar -borderwidth 1]
    pack $tb -side top -fill x
    button $tb.zoomin -image ::img::zoom-in -text [mc "Zoom in"] \
        -command "zoomConsole 1" -relief flat -width 30 -height 30
    button $tb.zoomout -image ::img::zoom-out -text [mc "Zoom out"] \
        -command "zoomConsole -1" -relief flat -width 30 -height 30
    pack $tb.zoomin $tb.zoomout -side left
}

proc SetPlotFormat { text } {
    if { $::xmaxima_default(plotwindow) == "embedded" } {
	sendMaxima $text ":lisp-quiet (prog2 (\$set_plot_option '((mlist simp) \$plot_format \$xmaxima)) nil) \n"
    }
}


