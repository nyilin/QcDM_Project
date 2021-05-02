QcDM Project
============

Overview
--------

The QcDM (**Q**uality **c**are for **D**iabetes **M**ellitus) Project provides
R-Shiny applications for generating glucometrics measures from point-of-care
blood glucose data at three different units of analysis: patient-sample,
patient-day, and patient-stay. The application facilitates convenient and
flexible generation of glucometrics measures by providing user-friendly
graphical user interfaces (GUIs) that runs on the user's desktop or from a USB
drive.

The QcDM Project consists of two R-Shiny GUIs and an R package. The first GUI,
QcDMconverter, processes raw glucose data files to conform to the data format
required by the application. The second GUI, QcDMui, is the main tool of the
QcDM Project that uses functions in the 
[QcDM package](https://github.com/nyilin/QcDM) to analyses processed glucose
data to generate glucometrics measures.

The application operates on Windows and Mac operating systems, and is provided
for free under the [MIT license](LICENSE). The current version of this
application can be downloaded from this repository.

Usage
-----

Detailed instructions on installation and usage of the QcDM Project is described
in **'QcDM Project User Manual.pdf'** in this repository. Briefly, users need to
first format the input BG data files according to the requirements of QcDM,
either manually or by using QcDMconverter, and then use QcDMui to generate
glucometrics reports. After launching QcDMui, users should first use the "Data"
tab to identify the folder that contains the data files to be processed,
configure the parameters to define glucometrics, and specify the time range to
generate glucometrics report for. The screen below shows an example
configuration:
<!--relative path did not work-->
![config](https://github.com/nyilin/QcDM_Project/blob/main/www/Figure2.jpg?raw=true) 

Next, users can proceed to the "Glucometrics" tab to view the report generated:

![config](https://github.com/nyilin/QcDM_Project/blob/main/www/Figure3.jpg?raw=true)
