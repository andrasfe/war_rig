---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS program (DFSRRC00) to delete expired authorizations using the BMP region controller. It specifies the program CBPAUP0C and PSB PSBPAUTB for the IMS execution. | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0 to a sequential dataset. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 with parameters to perform the unload. The... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the program to execute, the IMS libraries to use, and the GSAM datasets to be unloaded, along with other IMS... | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads the PAUTDB database in IMS. It first deletes the existing database files, then executes the DFSRRC00 program with the PAUDBUNL PSB to unload the database to new files. | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
