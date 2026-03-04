---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL job executes an IMS program (DFSRRC00) to delete expired authorizations using a BMP (Batch Message Processing) region. It specifies the program CBPAUP0C and PSB PSBPAUTB for the IMS... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0. It first deletes the output dataset if it exists, then executes the unload program (DFSRRC00) to extract the database definition. The job defines the... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database. It specifies the program to execute, the PSB and parameters, and defines the input and output datasets required for the... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS program DFSRRC00 to unload GSAM databases. It specifies the program to execute, the IMS libraries, and the input and output datasets required for the unload process. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads the PAUTDB database using the IMS program DFSRRC00. It deletes and recreates the root and child files, and executes the PAUDBUNL program to unload the database. | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
