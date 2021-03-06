USE [cdw]
GO
/****** Object:  Table [extract].[InstitutionsByYear]    Script Date: 11/27/2017 9:16:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [extract].[InstitutionsByYear](
	[Unitid] [int] NOT NULL,
	[YearId] [int] NOT NULL,
	[instnm] [nvarchar](100) NULL,
	[addr] [nvarchar](100) NULL,
	[city] [nvarchar](255) NULL,
	[stabbr] [nchar](2) NULL,
	[zip] [nchar](10) NULL,
	[fips] [int] NULL,
	[obereg] [smallint] NULL,
	[ein] [nchar](9) NULL,
	[opeid] [nchar](9) NULL,
	[opeflag] [smallint] NULL,
	[sector] [smallint] NULL,
	[iclevel] [smallint] NULL,
	[control] [smallint] NULL,
	[hloffer] [smallint] NULL,
	[hdegofr1] [smallint] NULL,
	[relaffil] [smallint] NULL,
	[locale] [smallint] NULL,
	[newid] [int] NULL,
	[deathyr] [smallint] NULL,
	[closedat] [nvarchar](16) NULL,
	[cbsa] [int] NULL,
	[cbsatype] [int] NULL,
	[csa] [int] NULL,
	[necta] [int] NULL,
	[countycd] [int] NULL,
	[longitud] [decimal](16, 10) NULL,
	[latitude] [decimal](16, 10) NULL,
	[f1syscod] [int] NULL,
	[ugoffer] [smallint] NULL,
	[groffer] [smallint] NULL,
	[deggrant] [smallint] NULL,
	[openpubl] [smallint] NULL,
	[landgrnt] [smallint] NULL,
	[hbcu] [smallint] NULL,
	[hospital] [smallint] NULL,
	[medical] [smallint] NULL,
	[tribal] [smallint] NULL,
	[slo5] [smallint] NULL,
	[confno1] [smallint] NULL,
	[confno2] [smallint] NULL,
	[cyactive] [smallint] NULL,
 CONSTRAINT [PK_InstitutionsByYear] PRIMARY KEY CLUSTERED 
(
	[Unitid] ASC,
	[YearId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
