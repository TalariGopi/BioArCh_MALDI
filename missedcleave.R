
 
missed.cleave.seq <- function (sequence, cuts = "K|R", skip = "X", massmin = 800, 
    massmax = 3500, verbose = F) 
{
    peptides <- NA
    pepidx <- 1
    len <- str_length(sequence)
    start <- 1
    for (pos in 1:len) {
        aa <- str_sub(sequence, pos, pos)
        if (str_detect(aa, cuts)) {
            ss <- str_sub(sequence, start, pos)
            if (str_detect(ss, skip)) {
                if (verbose) 
                  message(sprintf("  can't use %s", ss))
            }
            else {
                masses <- ms_tpeaks(ss)
                if (masses$mass[1] > massmin && masses$mass[1] < 
                  massmax) {
                  nhyd <- str_count(ss, "P")
                  nglut <- str_count(ss, "Q") + str_count(ss, 
                    "N")
                  for (hh in 0:nhyd) {
                    for (dd in 0:nglut) {
                      newrow <- data.frame(seq = as.character(ss), 
                        nhyd = hh, nglut = dd, mass1 = masses$mass[1] + 
                          (dd * 0.984015) + (hh * 16), seqpos = start)
                      if (pepidx == 1) {
                        peptides <- newrow
                      }
                      else {
                        peptides <- rbind(peptides, newrow)
                      }
                      pepidx <- pepidx + 1
                    }
                  }
                  if (verbose) 
                    message(sprintf("found sequence %s", ss))
                }
                else {
                  if (verbose) 
                    message(sprintf("  mass %0.2f out of range for sequence %s", 
                      masses$mass[1], ss))
                }
            }
            start <- pos + 1
        }
    }
    return(peptides)
}