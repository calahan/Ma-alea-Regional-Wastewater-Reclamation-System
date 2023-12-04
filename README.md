# MRWRS Floway
Design considerations for the Algal Turf Floway component of the Ma'alea Regional Wastewater Reclamation System  

## .gitignore
Empty Project does not normally track revisions in the directories Drafts, References, or Visual Elements. The expectation is that items in these folders are either generated and thus ephemeral (Drafts, Visual Elements), or are aliases referring to documents stored centrally (References).

## Visual Elements

### Figures
Figures are PNG files located in Visual Elements/Figures, named 1.png .. n.png, assembled from panels, which are PNG files located in subdirectories named consecutively starting with the numeral 1. The panels are named by consecutive capital letters, starting with A.png.

### Tables
Tables are PNG files numbered 1.png .. n.png, generated from data frames names 1 .. n.

## Automation
Eventually there will be a complete set of functions in the CalahanLab R packages to deal with situations such as making full copies of reference material or authoritative versions of generated documents.
