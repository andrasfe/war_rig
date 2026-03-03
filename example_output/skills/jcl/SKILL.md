---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS program (DFSRRC00) to delete expired authorizations. It defines the program to be executed, the parameters passed to it, and the necessary datasets for the IMS environment. | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads a DBD named DBPAUTP0. It first deletes the output dataset if it exists, then executes the unload process using the IMS program DFSRRC00. | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using the BMP (Batch Message Processing) region. It specifies the program, PSB, and input files required for the... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS program DFSRRC00 to unload GSAM databases. It specifies the program to execute, the required libraries, and the input and output datasets for the unload process. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads the PAUTDB database in IMS. It deletes existing database files, executes the DFSRRC00 utility to unload the database, and then recreates the database files. | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
