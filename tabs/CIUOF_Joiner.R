library(readxl)

# Compute the inner join of all 'Complaints, Inquiries, and Use of Force' datasets
# Results in an empty dataframe because no intersecting tuples
CIUOF_inner_join <- function() {
    # Read in data (ensures using most updated at time of join)    
    resist <- read_excel("data/CIUOF_Resistance.xlsx")
    demographics <- read_excel("data/CIUOF_Demographics.xlsx")
    allegations <- read_excel("data/CIUOF_Allegations.xlsx")
    type <- read_excel("data/CIUOF_Type.xlsx")
    
    # Inner join all dataframes
    df1 <- merge(x = resist, y = demographics, by = intersect(
                                               names(resist),
                                               names(demographics)
                                           ), all = FALSE)
    df2 <- merge(x = df1, y = allegations, by = intersect(
                                  names(df1),
                                  names(allegations)
                              ), all = FALSE)
    result <- merge(x = df2, y = type, by = intersect(
                                          names(df2),
                                          names(type)
                                      ), all = FALSE)

    # Clean up dataframes
    rm(df1)
    rm(df2)
    rm(type)
    rm(demographics)
    rm(resist)
    rm(allegations)

    # Return result dataframe
    return(result)
}

# Compute the outer join of all 'Complaints, Inquiries, and Use of Force' datasets
CIUOF_outer_join <- function() {
    # Read in data (ensures using most updated at time of join)
    resist <- read_excel("data/CIUOF_Resistance.xlsx")
    demographics <- read_excel("data/CIUOF_Demographics.xlsx")
    allegations <- read_excel("data/CIUOF_Allegations.xlsx")
    type <- read_excel("data/CIUOF_Type.xlsx")

    # Outer join all dataframes
    df1 <- merge(x = resist, y = demographics, by = intersect(
                                               names(resist),
                                               names(demographics)
                                           ),
                 all = TRUE)
    df2 <- merge(x = df1, y = allegations, by = intersect(
                                  names(df1),
                                  names(allegations)
                              ),
                 all = TRUE)
    result <- merge(x = df2, y = type, by = intersect(
                                          names(df2),
                                          names(type)
                                      ),
                    all = TRUE)

    # Clean up dataframes
    rm(df1)
    rm(df2)
    rm(type)
    rm(demographics)
    rm(resist)
    rm(allegations)

    # Return result dataframe
    return(result)
}

# Left Outer Join
CIUOF_left_outer_join <- function() {
    # Read in data (ensures using most updated at time of join)
    resist <- read_excel("data/CIUOF_Resistance.xlsx")
    demographics <- read_excel("data/CIUOF_Demographics.xlsx")
    allegations <- read_excel("data/CIUOF_Allegations.xlsx")
    type <- read_excel("data/CIUOF_Type.xlsx")

    # Outer join all dataframes by x
    df1 <- merge(x = resist, y = demographics, by = intersect(
                                               names(resist),
                                               names(demographics)
                                           ),
                 all.x = TRUE)
    df2 <- merge(x = df1, y = allegations, by = intersect(
                                  names(df1),
                                  names(allegations)
                              ),
                 all.x = TRUE)
    result <- merge(x = df2, y = type, by = intersect(
                                          names(df2),
                                          names(type)
                                      ),
                    all.x = TRUE)

    # Clean up dataframes
    rm(df1)
    rm(df2)
    rm(type)
    rm(demographics)
    rm(resist)
    rm(allegations)

    # Return result dataframe
    return(result)
}

# Right Outer Join
CIUOF_right_outer_join <- function() {
    # Read in data (ensures using most updated at time of join)
    resist <- read_excel("data/CIUOF_Resistance.xlsx")
    demographics <- read_excel("data/CIUOF_Demographics.xlsx")
    allegations <- read_excel("data/CIUOF_Allegations.xlsx")
    type <- read_excel("data/CIUOF_Type.xlsx")

    # Outer join all dataframes by y
    df1 <- merge(x = resist, y = demographics, by = intersect(
                                               names(resist),
                                               names(demographics)
                                           ),
                 all.y = TRUE)
    df2 <- merge(x = df1, y = allegations, by = intersect(
                                  names(df1),
                                  names(allegations)
                              ),
                 all.y = TRUE)
    result <- merge(x = df2, y = type, by = intersect(
                                          names(df2),
                                          names(type)
                                      ),
                    all.y = TRUE)

    # Clean up dataframes
    rm(df1)
    rm(df2)
    rm(type)
    rm(demographics)
    rm(resist)
    rm(allegations)

    # Return result dataframe
    return(result)
}

# Write out csvs for the dataframes of each join function
write_debug_csvs <- function() {
    # Inner join
    inner <- CIUOF_inner_join()
    # Outer join
    outer <- CIUOF_outer_join()
    # Left outer joint
    left <- CIUOF_left_outer_join()
    # Right outer join
    right <- CIUOF_right_outer_join()

    # Write dataframes to csvs
    write.csv(inner, "data/debug/inner.csv", row.names=TRUE)
    write.csv(outer, "data/debug/outer.csv", row.names=TRUE)
    write.csv(left, "data/debug/left.csv", row.names=TRUE)
    write.csv(right, "data/debug/right.csv", row.names=TRUE)

    # Clean up dataframes
    rm(inner)
    rm(outer)
    rm(left)
    rm(right)
}
