################################
# GUI for ade4 functions
################################
"ade4TkGUI" <- function(show = FALSE)
{
	require(tcltk) || stop("tcltk support is absent")
	require(ade4) || stop("ade4 support is absent")
	require(grDevices) || stop("grDevices support is absent")
	
	cmdlist <<- "cmdlist"
	winlist <<- 1
	if (exists("ade4TkGUIFlag")) rm(ade4TkGUIFlag, envir=.GlobalEnv)
#
# Main dialog window with title
#
	tt <- tktoplevel()
	tkwm.title(tt,"ade4TkGUI")
#
# Menu setup
#
	frame0 <- tkframe(tt, relief="groove", borderwidth=2, background="white")

	topMenuFile <- tkmenubutton(frame0, text="File", background="white")
	fileMenu <- tkmenu(topMenuFile, tearoff=TRUE)
	tkconfigure(topMenuFile, menu=fileMenu)
	tkadd(fileMenu,"command",label="Read text file",command=function() readtable(show))
	tkadd(fileMenu,"command",label="Load data set",command=function() choosepackage())
	tkadd(fileMenu,"command",label="Edit dataframe",command=function() editdf())
	tkadd(fileMenu,"separator")
	tkadd(fileMenu,"command",label="Quit R",command=function() askQuit())

	topMenuWin <- tkmenubutton(frame0, text="Windows", background="white")
	WinMenu <- tkmenu(topMenuWin, tearoff=TRUE)
	tkconfigure(topMenuWin, menu=WinMenu)
	tkadd(WinMenu,"command",label="New graphic window",command=function() newGr())
	tkadd(WinMenu,"command",label="Change graphic window",command=function() selectGr())
	tkadd(WinMenu,"command",label="Save graphic window",command=function() outgraph())

	topMenu1Tab <- tkmenubutton(frame0, text="1table", background="white")
	oneTabMenu <- tkmenu(topMenu1Tab, tearoff=TRUE)
	tkconfigure(topMenu1Tab, menu=oneTabMenu)
	tkadd(oneTabMenu,"command",label="Principal components analysis",command=function() dialog.dudi.pca(show))
	tkadd(oneTabMenu,"command",label="Correspondence analysis",command=function() dialog.dudi.coa(show))
	tkadd(oneTabMenu,"command",label="Multiple correspondence analysis",command=function() dialog.dudi.acm(show))
	tkadd(oneTabMenu,"command",label="Principal coordinates analysis",command=function() dialog.dudi.pco(show))
	tkadd(oneTabMenu,"command",label="Fuzzy correspondence analysis",command=function() dialog.dudi.fca(show))
	tkadd(oneTabMenu,"command",label="Fuzzy principal components analysis",command=function() dialog.dudi.fpca(show))
	tkadd(oneTabMenu,"command",label="Mixed (quant + qual) variables analysis",command=function() dialog.dudi.mix(show))
	tkadd(oneTabMenu,"command",label="Non symmetric correspondence analysis",command=function() dialog.dudi.nsc(show))
	tkadd(oneTabMenu,"command",label="Decentered correspondence analysis",command=function() dialog.dudi.dec(show))

	topMenu1TabG <- tkmenubutton(frame0, text="1table+groups", background="white")
	oneTabMenuG <- tkmenu(topMenu1TabG, tearoff=TRUE)
	tkconfigure(topMenu1TabG, menu=oneTabMenuG)
	tkadd(oneTabMenuG,"command",label="Between groups analysis",command=function() dialog.between(show))
	tkadd(oneTabMenuG,"command",label="Within groups analysis",command=function() dialog.within(show))
	tkadd(oneTabMenuG,"command",label="Discriminant analysis",command=function() dialog.discrimin(show))

	topMenu2Tab <- tkmenubutton(frame0, text="2tables", background="white")
	twoTabMenu <- tkmenu(topMenu2Tab, tearoff=TRUE)
	tkconfigure(topMenu2Tab, menu=twoTabMenu)
	tkadd(twoTabMenu,"command",label="Coinertia analysis",command=function() dialog.coinertia(show))
	tkadd(twoTabMenu,"command",label="Canonical correspondence analysis",command=function() dialog.cca(show))
	tkadd(twoTabMenu,"command",label="PCA w/r to instrumental variables",command=function() dialog.pcaiv(show))
	tkadd(twoTabMenu,"command",label="PCA w/r to orthogonal I.V.",command=function() dialog.pcaivortho(show))
	tkadd(twoTabMenu,"command",label="Double principal coordinate analysis",command=function() dialog.dpcoa(show))

	topMenuGraph <- tkmenubutton(frame0, text="Graphics", background="white")
	graphMenu <- tkmenu(topMenuGraph, tearoff=TRUE)
	tkconfigure(topMenuGraph, menu=graphMenu)
	tkadd(graphMenu,"command",label="Labels",command=function() dialog.s.label(show))
	tkadd(graphMenu,"command",label="Classes",command=function() dialog.s.class(show))
	tkadd(graphMenu,"command",label="Values",command=function() dialog.s.value(show))
	tkadd(graphMenu,"command",label="Correlation circle",command=function() dialog.s.corcircle(show))
	tkadd(graphMenu,"command",label="Convex hulls",command=function() dialog.s.chull(show))
	tkadd(graphMenu,"command",label="Match",command=function() dialog.s.match(show))
	tkadd(graphMenu,"separator")
	tkadd(graphMenu,"command",label="Display dudi",command=function() dudisp())
	tkadd(graphMenu,"command",label="Monte-Carlo test",command=function() dialog.MCTests())
	tkadd(graphMenu,"command",label="Explore graph",command=function() exploregraph())
	tkadd(graphMenu,"command",label="Reset graph list",command=function() resetgraph())

	tkpack(topMenuFile, topMenuWin, topMenu1Tab, topMenu1TabG, topMenu2Tab, topMenuGraph, side="left")
	tkpack(frame0, fill="x")
#
# title and icons
#
	frame1 <- tkframe(tt, relief="groove", borderwidth=2, background="white")

	for (i in 1:length(.libPaths())) {
		icnfnameR <- file.path(paste(.libPaths()[i],"/ade4TkGUI/Rlogo.gif",sep=""))
		if (file.exists(icnfnameR)) {
			icn <- tkimage.create("photo", file=icnfnameR)
			Rlabel <- tklabel(frame1, image=icn, background="white")
			oldopt <- options("htmlhelp")$htmlhelp
			tkbind(Rlabel, "<Button-1>", function() help.start())
			options("htmlhelp" = oldopt)
		}
		icnfnameTk <- file.path(paste(.libPaths()[i],"/ade4TkGUI/tcltk.gif",sep=""))
		if (file.exists(icnfnameTk)) {
			icn <- tkimage.create("photo", file=icnfnameTk)
			TclTklabel <- tklabel(frame1, image=icn, background="white")
			tkbind(TclTklabel, "<Button-1>", function() print(help("tcltk")))
		}
	}

	labh <- tklabel(frame1, bitmap="questhead", background="white")
	tkbind(labh, "<Button-1>", function() print(help("ade4TkGUI")))
	titre <- tklabel(frame1,text="ade4TkGUI", font="Times 20", foreground="red", background="white")
	tkgrid(Rlabel, titre, labh, TclTklabel, padx=20)
	tkpack(frame1, fill="x")
#
# read text files and load ade4 data set
#
	frame1b <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	tkgrid(tklabel(frame1b,text="- Data sets -", font="Times 14", foreground="blue", background="white"), columnspan=3)	
	readtable.but <- tkbutton(frame1b, text="Read a data file", command=function() readtable(show))
	getdata.but <- tkbutton(frame1b, text="Load a data set", command=function() choosepackage())
	tkgrid(readtable.but, getdata.but, ipadx=30)
	tkpack(frame1b, fill="x")

	tkbind(readtable.but, "<Button-3>", function() print(help("read.table")))
	tkbind(getdata.but, "<Button-3>", function() print(help("data")))

#
# One table analyses
#
	frame2 <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	tkgrid(tklabel(frame2,text="- One table analyses -", font="Times 14", foreground="blue", background="white"), columnspan=4)
	dudi.pca.but <- tkbutton(frame2, text="PCA", command=function() dialog.dudi.pca(show))
	dudi.coa.but <- tkbutton(frame2, text="COA", command=function() dialog.dudi.coa(show))
	dudi.acm.but <- tkbutton(frame2, text="MCA", command=function() dialog.dudi.acm(show))
	dudi.pco.but <- tkbutton(frame2, text="PCO", command=function() dialog.dudi.pco(show))
	#dudi.fca.but <- tkbutton(frame2, text="FCA", command=function() dialog.dudi.fca(show))
	#dudi.fpca.but <- tkbutton(frame2, text="FPCA", command=function() dialog.dudi.fpca(show))
	#dudi.mix.but <- tkbutton(frame2, text="Mix", command=function() dialog.dudi.mix(show))
	#dudi.nsc.but <- tkbutton(frame2, text="NSCA", command=function() dialog.dudi.nsc(show))
	#dudi.dec.but <- tkbutton(frame2, text="DCOA", command=function() dialog.dudi.dec(show))
	tkgrid(dudi.pca.but, dudi.coa.but, dudi.acm.but, dudi.pco.but, ipadx=20)
	#tkgrid(dudi.pco.but, dudi.fca.but, dudi.fpca.but, ipadx=20)
	#tkgrid(dudi.mix.but, dudi.nsc.but, dudi.dec.but, ipadx=20)
	tkpack(frame2, fill="x")

	tkbind(dudi.pca.but, "<Button-3>", function() print(help("dudi.pca")))
	tkbind(dudi.coa.but, "<Button-3>", function() print(help("dudi.coa")))
	tkbind(dudi.acm.but, "<Button-3>", function() print(help("dudi.acm")))
	tkbind(dudi.pco.but, "<Button-3>", function() print(help("dudi.pco")))
	#tkbind(dudi.fca.but, "<Button-3>", function() print(help("dudi.fca")))
	#tkbind(dudi.fpca.but, "<Button-3>", function() print(help("dudi.fpca")))
	#tkbind(dudi.mix.but, "<Button-3>", function() print(help("dudi.mix")))
	#tkbind(dudi.nsc.but, "<Button-3>", function() print(help("dudi.nsc")))
	#tkbind(dudi.dec.but, "<Button-3>", function() print(help("dudi.dec")))
#
# One table with groups
#
	frame2b <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	tkgrid(tklabel(frame2b,text="- One table with groups -", font="Times 14", foreground="blue", background="white"), columnspan=3)
	between.but <- tkbutton(frame2b, text="BGA", command=function() dialog.between(show))
	within.but <- tkbutton(frame2b, text="WGA", command=function() dialog.within(show))
	discrimin.but <- tkbutton(frame2b, text="DA", command=function() dialog.discrimin(show))
	tkgrid(between.but, within.but, discrimin.but, ipadx=20)
	tkpack(frame2b, fill="x")

	tkbind(between.but, "<Button-3>", function() print(help("between")))
	tkbind(within.but, "<Button-3>", function() print(help("within")))
	tkbind(discrimin.but, "<Button-3>", function() print(help("discrimin")))
#
# Two tables analyses
#
	frame2c <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	tkgrid(tklabel(frame2c,text="- Two tables analyses -", font="Times 14", foreground="blue", background="white"), columnspan=3)
	coinertia.but <- tkbutton(frame2c, text="Coinertia", command=function() dialog.coinertia(show))
	cca.but <- tkbutton(frame2c, text="CCA", command=function() dialog.cca(show))
	pcaiv.but <- tkbutton(frame2c, text="PCAIV", command=function() dialog.pcaiv(show))
	#pcaivortho.but <- tkbutton(frame2c, text="PCAIVortho", command=function() dialog.pcaivortho(show))
	tkgrid(coinertia.but, cca.but, pcaiv.but, ipadx=20)
	#tkgrid(pcaiv.but, pcaivortho.but, ipadx=20)
	tkpack(frame2c, fill="x")

	tkbind(coinertia.but, "<Button-3>", function() print(help("coinertia")))
	tkbind(cca.but, "<Button-3>", function() print(help("cca")))
	tkbind(pcaiv.but, "<Button-3>", function() print(help("pcaiv")))
	#tkbind(pcaivortho.but, "<Button-3>", function() print(help("pcaivortho")))
#
# Graphics
#
	frame3 <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	tkgrid(tklabel(frame3,text="- Graphic functions -", font="Times 14", foreground="blue", background="white"), columnspan=3)
	s.label.but <- tkbutton(frame3, text="Labels", command=function() dialog.s.label(show))
	#s.corcircle.but <- tkbutton(frame3, text="Cor. circle", command=function() dialog.s.corcircle(show))
	s.class.but <- tkbutton(frame3, text="Classes", command=function() dialog.s.class(show))
	s.value.but <- tkbutton(frame3, text="Values", command=function() dialog.s.value(show))
	#s.chull.but <- tkbutton(frame3, text="Convex hulls", command=function() dialog.s.chull(show))
	#s.match.but <- tkbutton(frame3, text="Match", command=function() dialog.s.match(show))
	tkgrid(s.label.but, s.class.but, s.value.but, ipadx=20)
	tkpack(frame3, fill="x")

	tkbind(s.label.but, "<Button-3>", function() print(help("s.label")))
	#tkbind(s.corcircle.but, "<Button-3>", function() print(help("s.corcircle")))
	tkbind(s.value.but, "<Button-3>", function() print(help("s.value")))
	tkbind(s.class.but, "<Button-3>", function() print(help("s.class")))
	#tkbind(s.chull.but, "<Button-3>", function() print(help("s.chull")))
	#tkbind(s.match.but, "<Button-3>", function() print(help("s.match")))
#
# Advanced graphics
#
	frame4 <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	tkgrid(tklabel(frame4,text="- Advanced graphics -", font="Times 14", foreground="blue", background="white"), columnspan=3)
	dudisp.but <- tkbutton(frame4, text="Display dudi", command=function() dudisp())
	explore.but <- tkbutton(frame4, text="Graph explore", command=function() exploregraph())
	#outGrDev.but <- tkbutton(frame4, text="save current graphic", command=function() outgraph())
	MC.but <- tkbutton(frame4, text="Monte-Carlo tests", command=function() dialog.MCTests(show))
	#newGr.but <- tkbutton(frame4, text="new graphic window", command=function() newGr())
	#selectGr.but <- tkbutton(frame4, text="change graphic window", command=function() selectGr())
	tkgrid(dudisp.but, MC.but, explore.but)
	#tkgrid(outGrDev.but, MC.but)
	#tkgrid(newGr.but, selectGr.but)
	tkpack(frame4, fill="x")

	tkbind(dudisp.but, "<Button-3>", function() print(help("dudi")))
#
# Quit
#
	frame5 <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	cancel.but <- tkbutton(frame5, text="Dismiss", command=function() tkdestroy(tt), foreground="darkgreen", background="white", font="Times 14")
	quity.but <- tkbutton(frame5, text="Quit R (save)", command=function() q("yes"), foreground="darkgreen", background="white", font="Times 14")
	quitn.but <- tkbutton(frame5, text="Quit R (don't save)", command=function() q("no"), foreground="darkgreen", background="white", font="Times 14")
	tkgrid(quity.but, cancel.but, quitn.but)
	tkpack(frame5, fill="x")
	tkfocus(tt)
	return(invisible())
}
