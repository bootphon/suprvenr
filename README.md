Tools for mapping continuous vector representations to discrete features
========================================================================

As featured in Dunbar, Synnaeve, and Dupoux (submitted 2015, ICPhS),
_Quantitative methods for comparing feature representations_ (henceforth
DSD15).

To reproduce the figures and tables from DSD15, you will need **R**, **knitR**,
and the **ggplot2** and **xtable** packages. The output is compatible with XeTeX.
Simply knit the file **icphs\_paper\_figures.Rnw**. You may need to replace or
remove the line:

    \setmainfont[Ligatures=TeX]{Times New Roman}

if you do not have a font file in your Latex font path with this name.
This is included so that phonetic characters display correctly. Path names
are relative to the **analysis** directory, so compile the document there.

Files
=====

 *  **ICPhS2015\_Feature\_Explorations.pdf:** a copy of DSD15

 *  **analysis/icphs\_paper\_figures.Rnw:** knitR document containing the code to
    generate the figures and tables from DSD15
 *  **analysis/visualize\_vector\_space.Rnw:** knitR document showing MDS of
    vector spaces, with manually devised classes from the paper highlighted
    (options "dev='cairo\_pdf'" and "font\_family='Times New Roman'" are
    there to make the phonetic characters show up with ggplot, but may cause
    problems on some systems)
