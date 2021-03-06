(defvar imaxima-mylatex
"%%%
%%% mylatex.ltx
%%%%%%%%%%%%%%%
%%%
%%% Use this file to make a format based on the preamble of any LaTeX
%%% file
%%%
%%% There are no restrictions on the distribution or modification of
%%% this file, except that other people should not attempt to alter
%%% the master copy on the ctan archives.
%%%
%%% Making the format
%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Given a LaTeX file `abc.tex', use initex as follows:
%%%
%%% initex &latex  mylatex.ltx abc      (Generic TeX)
%%% initex \\&latex mylatex.ltx abc      (unix and other TeX's)
%%% tex /i &latex  mylatex.ltx abc      (emtex)
%%%
%%% If you are on a Mac or using some shell that makes it inconvenient
%%% to use a command line such as the above examples then you may
%%% make a file `mylatex.tex' with the single line
%%% \\input mylatex.ltx abc
%%% and then pass the file mylatex.tex to your (ini)tex shell to produce
%%% the format, ie something equivalent to initex &latex mylatex.tex.
%%%
%%% If you are using OzTeX, see the separate instructions below.
%%%
%%% This should make a format file mylatex.fmt which you can then use
%%% as follows
%%%
%%% Using the new format
%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% tex  &mylatex abc          (generic TeX)
%%% virtex \\&mylatex abc       (Unix TeX)
%%%
%%% This will process your document, abc.tex, just as LaTeX does, but
%%% quicker as the contents of the preamble will be stored in the
%%% format file and will not need to be run each time.
%%%
%%% If (vir)tex fails to find your mylatex.fmt then it is not searching
%%% in the current directory, either modify your TEXFORMATS path (or
%%% equivalent) to search `.' or (on unix/dos) use   ./  as in:
%%% virtex \\&./mylatex abc
%%%
%%% Making and using the format with OzTeX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Given a LaTeX file `abc.tex', do this: select `TeX...' from OzTeX's 
%%% TeX menu, and go to the folder containing the file `abc.tex' as if you 
%%% were selecting the file `abc.tex'. Then hit the Cancel button - this 
%%% procedure sets OzTeX's working folder to the one containing abc.tex. 
%%% Next, select iniTeX from the TeX menu, and type:
%%% 
%%% &latex mylatex.ltx abc
%%% 
%%% This should make a format file mylatex.fmt which you can save in the 
%%% same folder as the file you're working on.
%%% 
%%% To use the new format, put this at the very start of the very first 
%%% line of your document:
%%% 
%%% %&mylatex
%%%
%%% Further Notes
%%%%%%%%%%%%%%%%%
%%%
%%% The file abc.tex must contain a line *just* with
%%% \\begin{document}
%%% Everything up to (but not including) the \\begin{document} will
%%% be saved in the format and not executed in subsequent runs.
%%%
%%% If you are modifying the document (or working on a similar document)
%%% you may wish to add new commands to your document preamble.
%%% The `mylatex' format normally skips the whole preamble (believing
%%% it to be pre-loaded) and so such new commands do not take effect.
%%% You could re-make the format, preloading the new preamble, but that
%%% might be inconvenient to do every time, and so an alternative scheme
%%% has been introduced.
%%% If the preamble contains a comment   mylatex  (ie a line just
%%% containing a % white space and the word mylatex) then the mylatex
%%% format will start reading the preable at that point so any new
%%% commands can be placed after such a comment and they will be
%%% executed.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% David Carlisle
%%%        1994/09/27
%%%
%%%  Modified 1994/10/21 after comments by
%%%           Volker Kunert <volker@numsun1.mathematik.uni-halle.de>
%%%  Modified 1996/01/26 To fix printout of preloaded files (which
%%%            hasnt worked since December 94 release, and to fix
%%%            incompatibility with \\makeindex command.
%%%  Modified 1997/10/04 To generalise the \\makeindex support to work
%%%            with any (well most:-) \\openout uses in the preamble.
%%%  Modified 1997/10/09 Most? Ross Moore pointed out I just broke
%%%            changebar, so further tinkering. Also useful discussions
%%%            with Ross lead to the following changes.
%%%            Added `mylatex' comment feature.
%%%            Added some font preloading.
%%%  Modified 1998/01/21 Jean-Francois Mertens pointed out that comments
%%%            before the first TeX command were broken by the previous
%%%            version. In particular %&mylatex special comments which
%%%            would be picked up by some TeX versions to automatically
%%%            select the format.
%%%  Modified 1998/10/14 Rowland McDonnell added notes on how to use 
%%%            mylatex.ltx with OzTeX
%%%  Modified 1999/01/04 Rowland McDonnell corrected notes on how to 
%%%            use mylatex.ltx with OzTeX.
%%%  Modified 2020/10/23 Phelype Oleinik updated to work with new hook code
%%%            in \\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\makeatletter

% Save the original definitions.
\\let\\MYLATEXdocument\\document
\\let\\MYLATEXopenout\\openout

% new for 2020-10-01
\\ifx\\@execute@begin@hook\\@undefined
\\message{Begin hook undefined}
\\else
\\message{Begin hook defined}
  \\let\\MYLATEX@execute@begin@hook\\@execute@begin@hook
  \\def\\@execute@begin@hook#1{}%
\\fi

% The version of \\document to use on the initex run.
% Just preloads some fonts, puts back \\document and \\openout,
% sets up the banner to display the file list of files preloaded,
% then sets up some special catcodes so the preamble will be
% skipped on normal runs with the new format.
\\def\\document{\\endgroup
% Force some font preloading.
 {\\setbox\\z@\\hbox{%
    $$% math (not bold, some setups don't have \\boldmath)
    \\normalfont% normal
    {\\ifx\\large\\@undefined\\else\\large\\fi % large and footnote
     \\ifx\\footnotesize\\@undefined\\else\\footnotesize\\fi}% 
    {\\bfseries\\itshape}% bold and bold italic
    {\\itshape}% italic
    \\ttfamily% monospace
    \\sffamily% sans serif
    }}% 
  \\let\\document\\MYLATEXdocument
  \\let\\openout\\MYLATEXopenout
  \\let\\@execute@begin@hook\\MYLATEX@execute@begin@hook
  \\makeatother
  \\everyjob\\expandafter{\\the\\everyjob
     \\begingroup
      \\listfiles
      \\expandafter\\MYLATEXcustomised\\@dofilelist
      \\endgroup}%
  \\@addtofilelist{.}%
  \\catcode`\\\\=13\\relax%
  \\catcode`\\#=12\\relax%
  \\catcode`\\ =9\\relax%
  \\dump}


% In principle \\openout stream= filename need not be space terminated,
% and need not be immediate, but this covers \\makeindex \\makeglossary
% and index package's \\newindex which are all the cases of \\openout
% that occur before \\begin{document} that I could see.
% Thanks to Ross Moore for pointing out \\AtBeginDocument is too late
% eg changebar package *closes* the stream in \\AtBeginDocument, so need
% to make sure it is opened before that. Make a special purpose hook.
% 
\\def\\openout#1 {%
  \\g@addto@macro\\MYLATEXopens{\\immediate\\openout#1 }}
\\let\\MYLATEXopens\\@empty

% Templates for ending the `preamble skipping process'.
\\def\\MYLATEXbegin{\\begin{document}}
\\def\\MYLATEXcomment{mylatex}

% Banner for \\everyjob.
\\def\\MYLATEXcustomised#1#2#3\\typeout#4{%
  \\typeout{CUSTOMISED FORMAT. Preloaded files:^^J%
  \\@spaces\\@spaces.}#3}

% While the preamble is being skipped, the EOL is active
% and defined to grab each line and inspect it looking
% for \\begin{document} or mylatex lines.
% The special catcodes required are not enabled until after the
% first TeX command in the file, so as to avoid problems with
% the special processing that TeX does on the first line, choosing
% the format, or the file name etc.
{\\catcode`\\^^M=\\active%
  \\catcode`\\/=0 %
  /catcode`\\\\=13 %
  /gdef\\{/catcode`/\\=0 /catcode`/^^M=13   /catcode`/%=9 ^^M}%
  /long/gdef^^M#1^^M{%
    /def/MYLATEXline{#1}%
% If hit a comment `mylatex' then do as if you'd hit \\begin{document}
% except don't run the real \\document as a \\begin{document} will be
% coming up later in the file at the end of the preamble.
    /ifx/MYLATEXline/MYLATEXcomment%
      /let/MYLATEXbegin/relax%
      /let/MYLATEXline/relax%
    /fi%
% If hit \\begin{document} put things back as they should be, run the
% hook with any save \\openouts then do the original \\document code.
    /ifx/MYLATEXline/MYLATEXbegin%
      /catcode`/^^M=5/relax%
      /let^^M/par%
      /catcode`/#=6/relax%
      /catcode`/%=14/relax%
      /catcode`/ =10/relax%
      /expandafter/MYLATEXopens/expandafter/MYLATEXbegin%
    /else%
% Otherwise grab the next line to look at.
      /expandafter^^M%
    /fi}}%

% Trick lookahead to allow mylatex.ltx and the document filename to be
% given on the same command line. (initex &latex mylatex.ltx abc.tex)
\\expandafter\\input\\endinput%

"
"Used by `imaxima-dump-tex'. If set to nil, then imaxima will assume that mylatex.ltx is installed on your system; otherwise, it creates mylatex.ltx from this source.

Source: David Carlisle's mylatex.ltx
https://github.com/davidcarlisle/dpctex/tree/master/mylatex" )

(provide 'mylatex.ltx)
