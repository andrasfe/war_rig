# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-25 18:30:17.971291

## Purpose

This JCL submits a batch job to execute the IMS DFSRRC00 program in BMP mode, running application program CBPAUP0C with PSB PSBPAUTB to delete expired authorizations. It defines necessary IMS libraries, control input, and output datasets.

**Business Context**: Supports CARDDEMO system by deleting expired authorizations from IMS database

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Inline control cards for IMS program specifying SSID 00, PCB 00001, TPB 00001, and option Y |
| STEPLIB | IOType.OTHER | IMS SDFSRESL and application LOADLIB for program execution |
| DFSRESLB | IOType.OTHER | IMS SDFSRESL resource library |
| PROCLIB | IOType.OTHER | IMS PROCLIB for procedures |
| DFSSEL | IOType.OTHER | IMS SDFSRESL selection library |
| IMS | IOType.OTHER | IMS PSBLIB and DBDLIB for PSB and DBD definitions |
| PSBPAUTB | IOType.IMS_SEGMENT | IMS database segments accessed via PSB PAUTB for authorization processing |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSOUX | IOType.REPORT | IMS extended output report |
| SYSOUT | IOType.REPORT | Standard job output |
| SYSABOUT | IOType.REPORT | IMS about information output |
| ABENDAID | IOType.REPORT | Abend aid output |
| SYSPRINT | IOType.REPORT | Print output from utilities |
| SYSUDUMP | IOType.REPORT | System dump output |
| IMSERR | IOType.REPORT | IMS error output |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS batch driver to execute CBPAUP0C in BMP mode with specified PSB |
| CBPAUP0C | CallType.DYNAMIC_CALL | Application program to delete expired authorizations |

## Business Rules

- **BR001**: Execute CBPAUP0C only with specific IMS subsystem (00), PCB (00001), TPB (00001), and option Y

## Open Questions

- ? Detailed function of control card values '00,00001,00001,Y' and their mapping to IMS resources
  - Context: Not explained in JCL comments; specific to IMS DFSRRC00 usage
- ? Specific IMS database and segments accessed by PSBPAUTB in CBPAUP0C
  - Context: PSB name implies authorization table but details not in JCL
