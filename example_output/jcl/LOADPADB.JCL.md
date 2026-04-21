# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-04-21 13:48:04.492985

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS environment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_SEQUENTIAL | Input file for the PAUTDB root segment. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_SEQUENTIAL | Input file for the PAUTDB child segment. |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library. |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB library version 151. |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library containing the IMS program. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM definition. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS program execution to load the PAUTDB database. |

## Paragraphs/Procedures

### LOADPADB
This JCL defines the job LOADPADB, which is responsible for executing an IMS batch program to load the PAUTDB database. The job specifies job characteristics such as job name, class, message class, message level, region size, notification ID, and time limit. It then executes the IMS program DFSRRC00 using the BMP region, specifying the PAUDBLOD transaction and PSBPAUTB PSB. The JCL defines the necessary STEPLIB DD statements to point to the IMS RESLIB and load libraries. It also defines the IMS DD statement to point to the PSBLIB and DBDLIB. The input files for the root and child segments of the PAUTDB database are defined using INFILE1 and INFILE2 DD statements, respectively. Finally, it defines the DFSVSAMP DD statement to point to the IMS VSAM definition and SYSOUT DD statements for SYSPRINT, SYSUDUMP, and IMSERR.
