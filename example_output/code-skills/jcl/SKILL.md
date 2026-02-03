---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes the IMS program DFSRRC00 to delete expired authorizations using the BMP region controller. It specifies CBPAUP0C as the application program and PSBPAUTB as the PSB (Program... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0 from an IMS database. It first deletes the output dataset if it exists, then executes the IMS Database Image Copy utility (DFSRRC00) to unload the DBD to a... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job unloads a GSAM database using the IMS program DFSRRC00. It specifies the program to execute, the libraries needed, and the input and output datasets for the GSAM database unload process. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads an IMS database (PAUTDB) using the IMS Database Utility (DFSRRC00). It deletes the existing database files, executes the unload utility with the specified PSB (PAUTBUNL), and... | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
