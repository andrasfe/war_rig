---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS program (DFSRRC00) to delete expired authorizations using a BMP (Batch Message Processing) region. It specifies the program CBPAUP0C and PSB (Program Specification Block)... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0. It first deletes the output dataset if it exists, then executes the unload program DFSRRC00 to extract the database definition. The job defines the necessary... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program DFSRRC00 to load the PAUTDB database using the BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes an IMS program (DFSRRC00) to unload a GSAM database. It specifies the program to execute, the parameters for the execution, and the datasets required for the IMS program to... | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads the PAUTDB database using the IMS program DFSRRC00. It first deletes the existing ROOT and CHILD files, then executes the unload utility to create new versions of these files. | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
