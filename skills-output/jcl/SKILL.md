---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS Batch Message Processing (BMP) program CBPAUP0C to delete expired authorizations. It defines the necessary datasets for the IMS environment and specifies the program... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 to unload the DBD to a sequential dataset. | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program DFSRRC00, the BMP region, the database name... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the program to execute, the required libraries, and the input and output datasets. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL unloads and redefines an IMS database. It first deletes the existing database files and then executes the DFSRRC00 utility to unload the database. Finally, it allocates new datasets for... | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
