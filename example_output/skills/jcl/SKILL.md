---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS program (DFSRRC00) to delete expired authorizations. It defines the execution environment for the IMS program, including specifying the program to run (CBPAUP0C) and the... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0 to a sequential dataset. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 to perform the unload. | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS program DFSRRC00 to unload GSAM databases. It specifies the IMS region, libraries, and input/output datasets required for the unload process. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads the PAUTDB database using the IMS program DFSRRC00. It first deletes existing database files and then recreates them after the unload. | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
