# bloodgascontent
Mathematical models for blood O2 saturation and content and CO2 content
2021-07-05, Matthias P. Hilty

The code has been created as part of a biomedical research project at the University of Zurich / University Hospital of Zurich:
https://www.researchgate.net/project/Extracorporeal-CO2-removal-in-hypercapnic-respiratory-failure
It may be used under the terms of the accompanying licence, while this repository must always be referenced.
Scientific publications using this work or parts of this work must cite this repository.

The R functions in this repository provide mathematical models for:
-Hemoglobin oxygen saturation
-Blood oxygen content
-Blood carbon dioxide content

The models approximate their dependent variable based on the main influencing factors as described in the following work:
* *Siggaard-Andersen O, Wimberley PD, Göthgen I, Siggaard-Andersen M. A mathematical model of the hemoglobin-oxygen dissociation curve of human blood and of the oxygen partial pressure as a function of temperature. Clin. Chem. 1984; 30: 1646–1651.*
* *Kelman GR. Digital computer procedure for the conversion of PCO2 into blood CO2 content. Respir Physiol 1967; 3: 111–115.*

Example R code is provided to derive standard O2 and CO2 content curves as a function of partial pressure at different pH ranges in ggplot2.
