---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL job CBPAUP0J executes the IMS region controller DFSRRC00 to run application program CBPAUP0C in BMP mode with PSB PAUTB for deleting expired authorizations from an IMS database. It... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL defines a batch job that first deletes any existing unload dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 and then executes the IMS unload utility DFSRRC00 to unload the DBPAUTP0 database using... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL defines a batch job named LOADPADB that executes the IMS utility program DFSRRC00 in BMP mode to load data into the PAUTDB IMS database. It reads root and child segment data from two... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS batch driver program DFSRRC00 to process the GSAM database DBUNLDGS using PSB DLIGSAMP under DLI control. It provides access to root and child GSAM input datasets for... | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job named UNLDPADB unloads IMS database PAUTDB root and child segments into sequential flat files using DFSRRC00 utility. STEP0 cleans up prior output files via IEFBR14 with delete... | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
