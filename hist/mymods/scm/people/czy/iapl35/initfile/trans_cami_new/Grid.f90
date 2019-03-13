module Grid
!------------------------------------------------------------------------------------------------
! Purpose: Parameters related to the dynamics grid
! Author : ZhangHe
! Completed : 2007.5.28
! Update: 2011-12-1
!------------------------------------------------------------------------------------------------
   implicit none

   save
   public

! for gaussian grid
   integer, parameter :: GLAT = 64
   integer, parameter :: GLON = 128
   integer, parameter :: GLEV = 30
   integer, parameter :: GLVP = 31
   integer, parameter :: GLATON = GLON*GLAT
! for lat-lon grid
   integer, parameter :: NLAT = 128         
   integer, parameter :: NLON = 256
!   integer, parameter :: NLEV = GLEV
!   integer, parameter :: NLEV = 51
   integer, parameter :: NLEV = 35
!   integer, parameter :: NLVP = GLVP
!   integer, parameter :: NLVP = 52
   integer, parameter :: NLVP = 36
! for all
   integer, parameter :: NTIM = 1
   integer, parameter :: NCHR = 1
   integer, parameter :: NDIMS = 4
!
   integer, parameter :: JB = 2         !zhh 2007.10.11
   integer, parameter :: JE = NLAT - 1
                       
end module Grid
