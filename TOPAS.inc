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
