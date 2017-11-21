\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{neutron}[2017]

\LoadClass[a4paper]{article}

\RequirePackage[hmargin=3cm,vmargin=2cm]{geometry}

\RequirePackage{fancyhdr}
\renewcommand{\headrulewidth}{0pt}
\pagestyle{fancy}
\fancyhf{}
\lfoot{\thepage~(\@date)}

\renewcommand{\maketitle}{%
  \noindent{\huge{\bfseries{\@title}}\hrulefill}

  \medskip

  \noindent\@author

  \bigskip
}
