---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL defines a batch job to execute the IMS batch message processing (BMP) driver DFSRRC00, invoking application program CBPAUP0C with PSB PSBPAUTB to delete expired authorizations. It sets up... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job first deletes any existing dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 using IEFBR14 and then executes the IMS unload utility DFSRRC00 to unload the entire IMS database DBPAUTP0 into a... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | The LOADPADB JCL job executes the IMS reconstruction utility DFSRRC00 to load/reconstruct the PAUTDB IMS database in BMP mode using PSB PAUTB. It reads root segments from INFILE1 and child... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS GSAM database unload utility DFSRRC00 to unload the PAUTDB database (segments ROOT and CHILD from GSAM datasets). It allocates necessary IMS libraries, DBD libraries,... | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job executes an IMS database unload for the PAUTDB database, producing sequential flat files for root and child segments. STEP0 uses IEFBR14 to delete any existing output files. STEP01... | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
