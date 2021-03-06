USE [cdw]
GO
/****** Object:  Table [extract].[NsfIpedsBridge]    Script Date: 11/27/2017 9:16:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [extract].[NsfIpedsBridge](
	[InstitutionId] [int] NOT NULL,
	[RdFice] [int] NULL,
	[Fice] [int] NULL,
	[Unitid] [int] NULL,
	[RollupId] [int] NULL,
	[NsfSurveyId] [int] NULL,
	[NsfStartingInstitutionCode] [int] NULL,
	[Institution Name] [nvarchar](255) NOT NULL,
	[State] [nchar](2) NOT NULL,
	[Type of Control] [nchar](3) NOT NULL,
	[Highest Degree] [nchar](3) NOT NULL,
	[IsHbcu] [bit] NOT NULL,
	[IsMedical] [bit] NOT NULL,
	[LatestYear] [int] NOT NULL
) ON [PRIMARY]

GO
