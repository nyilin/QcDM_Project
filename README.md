QcDM Project
============

Overview
--------

The QcDM (<ins>Q</ins>uality <ins>c</ins>are for <ins>D</ins>iabetes 
<ins>M</ins>ellitus) Project provides R-Shiny applications for generating
glucometrics measures from point-of-care blood glucose data at three different
units of analysis: patient-sample, patient-day, and patient-stay. The
application facilitates convenient and flexible generation of glucometrics
measures by providing user-friendly graphical user interfaces (GUIs) that runs
in a browser on the user's desktop.

The QcDM Project consists of two R-Shiny GUIs and an R package. The first GUI,
QcDMconverter, processes raw glucose data files to conform to the data format
required by the application. The second GUI, QcDMui, is the main tool of the
QcDM Project that uses functions in the 
[QcDM package](https://github.com/nyilin/QcDM) to analyses processed glucose
data to generate glucometrics measures.

The application operates on Windows and Mac operating systems, and is provided
for free under an academic non-commercial license. The current version of this
application can be downloaded from this repository. Detailed instructions on
installation and usage of the QcDM Project is described in 
**'QcDM Project User Manual.pdf'** in this repository.
