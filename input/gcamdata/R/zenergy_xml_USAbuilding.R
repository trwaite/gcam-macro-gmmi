# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_USAbuilding_xml
#'
#' Construct XML data structure for \code{building_det_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_det_USA.xml}. The corresponding file in the
#' original data system was \code{batch_building_det_USA.xml} (gcamusa XML).
module_energy_USAbuilding_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A44.demand_satiation_mult",
             "L244.DeleteConsumer_USAbld",
             "L244.DeleteSupplysector_USAbld",
             "L244.SubregionalShares_gcamusa",
             "L244.PriceExp_IntGains_gcamusa",
             "L244.Floorspace_gcamusa",
             "L244.DemandFunction_serv_gcamusa",
             "L244.DemandFunction_flsp_gcamusa",
             "L244.Satiation_flsp_gcamusa",
             "L244.SatiationAdder", # satiation adder pulls from global buildings
             "L244.ThermalBaseService_gcamusa",
             "L244.GenericBaseService_gcamusa",
             "L244.Intgains_scalar", # intgains scaler pulls from global buildings
             "L244.ShellConductance_bld_gcamusa",
             "L244.Supplysector_bld_gcamusa",
             "L244.FinalEnergyKeyword_bld_gcamusa",
             "L244.SubsectorShrwtFllt_bld_gcamusa",
             "L244.SubsectorInterp_bld_gcamusa",
             "L244.SubsectorInterpTo_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa",
             "L244.StubTech_bld_gcamusa",
             "L244.StubTechCalInput_bld",
             "L244.StubTechCalInput_bld_gcamusa",
             #"L244.StubTechMarket_bld", market-name defaults to the USA region; no need for this table
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GompFnParam", # gompertz function parameters are pulled from global buildings
             "L244.GlobalTechSCurve_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_det_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.demand_satiation_mult <- get_data(all_data, "gcam-usa/A44.demand_satiation_mult")
    L244.DeleteConsumer_USAbld <- get_data(all_data, "L244.DeleteConsumer_USAbld")
    L244.DeleteSupplysector_USAbld <- get_data(all_data, "L244.DeleteSupplysector_USAbld")
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares_gcamusa")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains_gcamusa")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace_gcamusa")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv_gcamusa")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp_gcamusa")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp_gcamusa")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService_gcamusa")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService_gcamusa")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld_gcamusa")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld_gcamusa")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamusa")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamusa")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld_gcamusa")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamusa")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld_gcamusa")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld_gcamusa")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld")
    L244.StubTechCalInput_bld_gcamusa <- get_data(all_data, "L244.StubTechCalInput_bld_gcamusa")
    L244.GlobalTechIntGainOutputRatio <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio")
    L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamusa")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld_gcamusa")
    L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld")
    L244.GompFnParam<- get_data(all_data, "L244.GompFnParam")

    # ===================================================

    # This code (1) removes the existing USA buildings supplysectors and gcam-consumers, (2) compiles the GCAM-USA
    # set of input tables used to create the building_USA.xml file, and aggregates/filters as appropriate
    # in order to construct a detailed buildings sector, similar to what is used in GCAM-USA for the USA region as a
    # whole

    # simple function, may be added to over time. right now it's just a filtering function, used for generic tables.
    process_state_data_to_USA <- function(data_table, type = "generic"){
      # If it's a generic table, where all states have the same values, we can just drop in the USA region
      if(type == "generic"){
        rev_data <- data_table %>%
          mutate(region = "USA") %>%
          distinct()
      }
    }

    # The following tables are generic
    L244.SubregionalShares_USAbld <- process_state_data_to_USA(L244.SubregionalShares)
    L244.PriceExp_IntGains_USAbld <- process_state_data_to_USA(L244.PriceExp_IntGains)
    L244.DemandFunction_serv_USAbld <- process_state_data_to_USA(L244.DemandFunction_serv)
    L244.DemandFunction_flsp_USAbld <- process_state_data_to_USA(L244.DemandFunction_flsp)
    L244.Satiation_flsp_USAbld <- process_state_data_to_USA(L244.Satiation_flsp)
    L244.ShellConductance_USAbld <- process_state_data_to_USA(L244.ShellConductance_bld)
    L244.Supplysector_USAbld <- process_state_data_to_USA(L244.Supplysector_bld)
    L244.FinalEnergyKeyword_USAbld <- process_state_data_to_USA(L244.FinalEnergyKeyword_bld)
    L244.SubsectorShrwtFllt_USAbld <- process_state_data_to_USA(L244.SubsectorShrwtFllt_bld)
    L244.SubsectorInterp_USAbld <- process_state_data_to_USA(L244.SubsectorInterp_bld)
    L244.SubsectorInterpTo_USAbld <- process_state_data_to_USA(L244.SubsectorInterpTo_bld)
    L244.SubsectorLogit_USAbld <- process_state_data_to_USA(L244.SubsectorLogit_bld)
    L244.StubTech_USAbld <- process_state_data_to_USA(L244.StubTech_bld)

    # The following tables pull their info from the core model table, filtering the USA region
    L244.SatiationAdder_USAbld <- filter(L244.SatiationAdder, region == "USA")
    L244.GompFnParam_USAbld <- filter(L244.GompFnParam, region == "USA")
    L244.Intgains_scalar_USAbld <- filter(L244.Intgains_scalar, region == "USA")

    # The remaining data tables require some individual processing
    # Floorspace is the sum of the 50 states
    L244.Floorspace_USAbld <- L244.Floorspace %>%
      mutate(region = "USA") %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, year) %>%
      summarise(base.building.size = sum(base.building.size)) %>%
      ungroup()

    # Adjustments to calibration: energy-for-water is in the GCAM-USA building sector (comm non-building)
    # The energy and service demands here need to be revised to accommodate these differences
    # The code below assigns all EFW electricity to "comm non-building", and the other fuels (desalination) are
    # assigned to "comm other" (comm non-building only consumes electricity)
    L244.USABuildingEnergy <- filter(L244.StubTechCalInput_bld, region == "USA") %>%
      group_by(year, minicam.energy.input) %>%
      summarise(USATotal = sum(calibrated.value)) %>%
      ungroup()

    L244.BuildingEnergyDifference <- L244.StubTechCalInput_bld_gcamusa %>%
      group_by(year, minicam.energy.input) %>%
      summarise(GCAMUSATotal = sum(calibrated.value)) %>%
      ungroup() %>%
      left_join(L244.USABuildingEnergy, by = c("year", "minicam.energy.input")) %>%
      mutate(difference = GCAMUSATotal - USATotal) %>%
      filter(difference > 1e-6) %>%
      mutate(supplysector = if_else(minicam.energy.input == "elect_td_bld", "comm non-building", "comm other")) %>%
      select(supplysector, year, minicam.energy.input, difference)

    # Energy efficiencies are indexed (set equal to 1) in base years for these services, so the sum of service output
    # by each service is equal to energy consumption. Need to add all of the fuels together
    L244.BuildingServiceDifference <- L244.BuildingEnergyDifference %>%
      rename(building.service.input = supplysector) %>%
      group_by(building.service.input, year) %>%
      summarise(difference = sum(difference)) %>%
      ungroup()

    # Base service of each service is the sum of all states
    L244.ThermalBaseService_USAbld <- L244.ThermalBaseService %>%
      mutate(region = "USA") %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, thermal.building.service.input, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup()

    # Generic base service needs to be adjusted for the change in energy consumption
    L244.GenericBaseService_USAbld <- L244.GenericBaseService %>%
      mutate(region = "USA") %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, building.service.input, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      left_join(L244.BuildingServiceDifference, by = c("building.service.input", "year")) %>%
      replace_na(list(difference = 0)) %>%
      mutate(base.service = base.service - difference) %>%
      select(-difference)

    # Service satiation is calculated as base service divided by floorspace multiplied by the exogenous
    # service satiation multiplier from A44.demand_satiation_mult
    L244.ThermalServiceSatiation_USAbld <- L244.ThermalBaseService_USAbld %>%
      filter(year == max(year)) %>%
      left_join_error_no_match(L244.Floorspace_USAbld,
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      left_join_error_no_match(A44.demand_satiation_mult,
                               by = c("thermal.building.service.input" = "supplysector")) %>%
      mutate(base.level = base.service / base.building.size,
             satiation.level = round(base.level * multiplier, energy.DIGITS_SATIATION_ADDER))  %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    L244.GenericServiceSatiation_USAbld <- L244.GenericBaseService_USAbld %>%
      filter(year == max(year)) %>%
      left_join_error_no_match(L244.Floorspace_USAbld,
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      left_join_error_no_match(A44.demand_satiation_mult,
                               by = c("building.service.input" = "supplysector")) %>%
      mutate(base.level = base.service / base.building.size,
             satiation.level = round(base.level * multiplier, energy.DIGITS_SATIATION_ADDER)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])


    # Energy calibration (L244.StubTechCalInput_bld_gcamusa): sum of all states' energy consumption. share-weights take the maximum
    L244.StubTechCalInput_USAbld <- L244.StubTechCalInput_bld_gcamusa %>%
      mutate(region = "USA") %>%
      group_by(region, supplysector, subsector, stub.technology, year, minicam.energy.input, share.weight.year) %>%
      summarise(calibrated.value = sum(calibrated.value),
                subs.share.weight = max(subs.share.weight),
                tech.share.weight = max(tech.share.weight)) %>%
      left_join(L244.BuildingEnergyDifference, by = c("supplysector", "year", "minicam.energy.input")) %>%
      replace_na(list(difference = 0)) %>%
      mutate(calibrated.value = calibrated.value - difference) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])


    # Produce outputs
    create_xml("building_det_USA.xml") %>%
      add_xml_data(L244.DeleteConsumer_USAbld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_USAbld, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares_USAbld, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains_USAbld, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace_USAbld, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv_USAbld, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp_USAbld, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp_USAbld, "Satiation_flsp") %>%
      add_xml_data(L244.SatiationAdder_USAbld, "SatiationAdder") %>%
      add_xml_data(L244.GompFnParam_USAbld, "GompFnParam")  %>%
      add_xml_data(L244.ThermalBaseService_USAbld, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService_USAbld, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation_USAbld, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation_USAbld, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar_USAbld, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_USAbld, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_USAbld, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_USAbld, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_USAbld, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_USAbld, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.SubsectorLogit_USAbld, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_USAbld, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_USAbld, "StubTechCalInput") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_bld, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_bld, "GlobalTechSCurve")  %>%
      add_precursors("gcam-usa/A44.demand_satiation_mult",
                     "L244.DeleteConsumer_USAbld",
                     "L244.DeleteSupplysector_USAbld",
                     "L244.SubregionalShares_gcamusa",
                     "L244.PriceExp_IntGains_gcamusa",
                     "L244.Floorspace_gcamusa",
                     "L244.DemandFunction_serv_gcamusa",
                     "L244.DemandFunction_flsp_gcamusa",
                     "L244.Satiation_flsp_gcamusa",
                     "L244.SatiationAdder",
                     "L244.ThermalBaseService_gcamusa",
                     "L244.GenericBaseService_gcamusa",
                     "L244.Intgains_scalar",
                     "L244.ShellConductance_bld_gcamusa",
                     "L244.Supplysector_bld_gcamusa",
                     "L244.FinalEnergyKeyword_bld_gcamusa",
                     "L244.SubsectorShrwtFllt_bld_gcamusa",
                     "L244.SubsectorInterp_bld_gcamusa",
                     "L244.SubsectorInterpTo_bld_gcamusa",
                     "L244.SubsectorLogit_bld_gcamusa",
                     "L244.StubTech_bld_gcamusa",
                     "L244.StubTechCalInput_bld",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L244.GlobalTechIntGainOutputRatio",
                     "L244.GlobalTechInterpTo_bld",
                     "L244.GlobalTechEff_bld",
                     "L244.GlobalTechShrwt_bld_gcamusa",
                     "L244.GlobalTechCost_bld_gcamusa",
                     "L244.GompFnParam",
                     "L244.GlobalTechSCurve_bld") ->
      building_det_USA.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorInterpTo_USAbld)) {

      building_det_USA.xml %>%
        add_xml_data(L244.SubsectorInterpTo_USAbld, "SubsectorInterpTo") ->
        building_det_USA.xml

      }

    return_data(building_det_USA.xml)
  } else {
    stop("Unknown command")
  }
}
