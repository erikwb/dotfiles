 (define-skeleton latex-letter-skeleton
   "Inserts a Latex letter skeleton into current buffer.
 This only makes sense for empty buffers."
   "Salutation: "
   "\\documentclass[a4paper]{letter}\n"
   "\\usepackage[latin1]{inputenc}\n"
   "\\name{Erik Bourget}\n"
   "\\address{Erik Bourget \\\\ 3440 Durocher \\#1816 \\\\ Montreal, PQ H2X 2E2}\n"
   "\\begin{document}\n"
   "\\begin{letter}{" str | " *** salutation *** " "}\n"
   "\\opening{" _ "}\n\n"
   "\\closing{Sincerely,}\n"
   "\\end{letter}\n"
   "\\end{document}\n")

(define-skeleton latex-paper-skeleton
  "Inserts a Latex paper skeleton into current buffer."
  "Title: "
  "% Hi, Emacs. -*- Mode: LATEX -*-\n"
  "\n"
  "\\documentclass[12pt]{article}\n"
  "% Formatting parameters\n"
  "\\topmargin -0.15in\n"
  "\\headheight 0in\n"
  "\\headsep 0.5in\n"
  "\\textheight 8.75in\n"
  "\\textwidth 6.5in\n"
  "\\oddsidemargin 0in\n"
  "\\evensidemargin 0in\n"
  "\n"
  "% Line spacing\n"
  "\\renewcommand{\\baselinestretch}{1.0}\n"
  "\n"
  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
  "\\begin{document}\n"
  "%\\thispagestyle{empty}\n"
  "\\title{\\bf" str | " *** Title *** " "}"
  "\\date{Date}\n"
  "\n"
  "\\author{\n"
  "\\bf Erik Bourget\\\\\n"
  "email: ebourg@cs.mcgill.ca\\\\\n"
  "\\\\\n"
  "}\n"
  "\n"
  "\\maketitle\n"
  "\\thispagestyle{empty}\n"
  "\n"
  "\\newpage\n"
  "%---------------------------------------------------------------------\n"
  "\n"
  "\\end{document}\n"
  "\n"
  "%% Local Variables:\n"
  "%% TeX-master: t\n"
  "%% TeX-command-default: \"LaTeX2e\"\n"
  "%% mode: auto-fill\n"
  "%% fill-column: 79\n"
  "%% End:\n")

(define-skeleton c-comment-skeleton
  "Inserts a little C comment."
  ""
  "/*\n"
  " * " _ "\\n"
  " */\n")
