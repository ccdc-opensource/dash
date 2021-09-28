/*
Copyright 2007 Cambridge Crystallographic Data Centre

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

macro Out_CIF_STR_Uiso(file)
{
    out file
       Out_String("\ndata_")
       Out(Get(phase_name), "\n_chemical_name_mineral ?%s?")
       Out(Get(a), "\n_cell_length_a  %V")
       Out(Get(b), "\n_cell_length_b  %V")
       Out(Get(c), "\n_cell_length_c  %V")
       Out(Get(al), "\n_cell_angle_alpha %V")
       Out(Get(be), "\n_cell_angle_beta  %V")
       Out(Get(ga), "\n_cell_angle_gamma %V")
       Out(Get(cell_volume), "\n_cell_volume %V")

       Out(Get(sp_grp_char), "\n_space_group %s")

       Out_String("\nloop_\n_symmetry_equiv_pos_as_xyz")
       Out(Get(sp_xyzs_txt), "%s")

       Out_String("\nloop_")
          Out_String("\n_atom_site_label")
          Out_String("\n_atom_site_type_symbol")
          Out_String("\n_atom_site_symmetry_multiplicity")
          Out_String("\n_atom_site_fract_x")
          Out_String("\n_atom_site_fract_y")
          Out_String("\n_atom_site_fract_z")
          Out_String("\n_atom_site_occupancy")
          Out_String("\n_atom_site_B_iso_or_equiv")
          Out_String("\n_atom_site_U_iso_or_equiv")
          Out_String("\n")
          atom_out file append
             load out_record out_fmt out_eqn
             {
                "\n%s" = Get_From_String(Get(current_atom), site);
                " %s" = Get_From_String(Get(current_atom), atom);
                " %3.0f" = Get_From_String(Get(current_atom), num_posns);
                " %V" = Get_From_String(Get(current_atom), x);
                " %V" = Get_From_String(Get(current_atom), y);
                " %V" = Get_From_String(Get(current_atom), z);
                " %V" = Get_From_String(Get(current_atom), occ);
                " %V" = Get_From_String(Get(current_atom), beq);
                " %11.5f" = (1.0/(8.0 * Pi * Pi)) * Get_From_String(Get(current_atom), beq);
             }
}
macro Out_X_Yobs_Ycalc_and_Difference_divide_by_SigmaYobs(file)
{
   xdd_out file load out_record out_fmt out_eqn
   {
   " %11.5f " = X;
   " %11.5f " = Yobs;
   " %11.5f " = Ycalc;
   " %11.5f\n" = (Yobs-Ycalc)/SigmaYobs;
   }
}
'******************************************************************************

macro Out_Profile(file)
{
xdd_out file load out_record out_fmt out_eqn
{
" %11.5f " = X;
" %11.5f " = Yobs;
" %11.5f " = Ycalc;
" %11.5f\n" = SigmaYobs;
}
}

'******************************************************************************

' Output a file with a series of points at the reflection positions
macro Out_Tick(file)
{
phase_out file load out_record out_fmt out_eqn
{
" %11.5f -200\n" = 2 Rad Th;
}
}

'******************************************************************************

' Output a file suitable for CIF submission
macro Out_PowderDataBlock(file)
{
out file
Out_String("loop_")
Out_String("\n_pd_meas_2theta_scan")
Out_String("\n_pd_proc_intensity_total")
Out_String("\n_pd_calc_intensity_total")
Out_String("\n_pd_proc_ls_weight")
Out_String("\n")
xdd_out file append load out_record out_fmt out_eqn
{
" %11.5f " = X;
" %11.5f " = Yobs;
" %11.5f " = Ycalc;
" %11.5f\n" = 1./(SigmaYobs*SigmaYobs);
}
}

'******************************************************************************
