\name{aaindex}
\alias{aaindex}
\docType{data}
\title{List of 544 physicochemical and biological properties for the 20 amino-acids}
\description{Data were imported from release 9.1 (AUG 2006) of the aaindex1
database. See the reference section to cite this database in a publication.
}
\usage{data(aaindex)}
\format{
  A named list with 544 elements having each the following components:
  \describe{
    \item{H}{String: Accession number in the aaindex database.}
    \item{D}{String: Data description.}
    \item{R}{String: LITDB entry number.}
    \item{A}{String: Author(s).}
    \item{T}{String: Title of the article.}
    \item{J}{String: Journal reference and comments.}
    \item{C}{String: Accession numbers of similar entries with the correlation 
    coefficients of 0.8 (-0.8) or more (less). Notice: The correlation 
    coefficient is calculated with zeros filled for missing values.}
    \item{I}{Numeric named vector: amino acid index data.}
  }
}
\details{
A short description of each entry is available under the D component:\cr

alpha-CH chemical shifts (Andersen et al., 1992)\cr
Hydrophobicity index (Argos et al., 1982)\cr
Signal sequence helical potential (Argos et al., 1982)\cr
Membrane-buried preference parameters (Argos et al., 1982)\cr
Conformational parameter of inner helix (Beghin-Dirkx, 1975)\cr
Conformational parameter of beta-structure (Beghin-Dirkx, 1975)\cr
Conformational parameter of beta-turn (Beghin-Dirkx, 1975)\cr
Average flexibility indices (Bhaskaran-Ponnuswamy, 1988)\cr
Residue volume (Bigelow, 1967)\cr
Information value for accessibility; average fraction 35% (Biou et al., 1988)\cr
Information value for accessibility; average fraction 23% (Biou et al., 1988)\cr
Retention coefficient in TFA (Browne et al., 1982)\cr
Retention coefficient in HFBA (Browne et al., 1982)\cr
Transfer free energy to surface (Bull-Breese, 1974)\cr
Apparent partial specific volume (Bull-Breese, 1974)\cr
alpha-NH chemical shifts (Bundi-Wuthrich, 1979)\cr
alpha-CH chemical shifts (Bundi-Wuthrich, 1979)\cr
Spin-spin coupling constants 3JHalpha-NH (Bundi-Wuthrich, 1979)\cr
Normalized frequency of alpha-helix (Burgess et al., 1974)\cr
Normalized frequency of extended structure (Burgess et al., 1974)\cr
Steric parameter (Charton, 1981)\cr
Polarizability parameter (Charton-Charton, 1982)\cr
Free energy of solution in water, kcal/mole (Charton-Charton, 1982)\cr
The Chou-Fasman parameter of the coil conformation (Charton-Charton, 1983)\cr
A parameter defined from the residuals obtained from the best correlation of the Chou-Fasman parameter of beta-sheet (Charton-Charton, 1983)\cr
The number of atoms in the side chain labelled 1+1 (Charton-Charton, 1983)\cr
The number of atoms in the side chain labelled 2+1 (Charton-Charton, 1983)\cr
The number of atoms in the side chain labelled 3+1 (Charton-Charton, 1983)\cr
The number of bonds in the longest chain (Charton-Charton, 1983)\cr
A parameter of charge transfer capability (Charton-Charton, 1983)\cr
A parameter of charge transfer donor capability (Charton-Charton, 1983)\cr
Average volume of buried residue (Chothia, 1975)\cr
Residue accessible surface area in tripeptide (Chothia, 1976)\cr
Residue accessible surface area in folded protein (Chothia, 1976)\cr
Proportion of residues 95% buried (Chothia, 1976)\cr
Proportion of residues 100% buried (Chothia, 1976)\cr
Normalized frequency of beta-turn (Chou-Fasman, 1978a)\cr
Normalized frequency of alpha-helix (Chou-Fasman, 1978b)\cr
Normalized frequency of beta-sheet (Chou-Fasman, 1978b)\cr
Normalized frequency of beta-turn (Chou-Fasman, 1978b)\cr
Normalized frequency of N-terminal helix (Chou-Fasman, 1978b)\cr
Normalized frequency of C-terminal helix (Chou-Fasman, 1978b)\cr
Normalized frequency of N-terminal non helical region (Chou-Fasman, 1978b)\cr
Normalized frequency of C-terminal non helical region (Chou-Fasman, 1978b)\cr
Normalized frequency of N-terminal beta-sheet (Chou-Fasman, 1978b)\cr
Normalized frequency of C-terminal beta-sheet (Chou-Fasman, 1978b)\cr
Normalized frequency of N-terminal non beta region (Chou-Fasman, 1978b)\cr
Normalized frequency of C-terminal non beta region (Chou-Fasman, 1978b)\cr
Frequency of the 1st residue in turn (Chou-Fasman, 1978b)\cr
Frequency of the 2nd residue in turn (Chou-Fasman, 1978b)\cr
Frequency of the 3rd residue in turn (Chou-Fasman, 1978b)\cr
Frequency of the 4th residue in turn (Chou-Fasman, 1978b)\cr
Normalized frequency of the 2nd and 3rd residues in turn (Chou-Fasman, 1978b)\cr
Normalized hydrophobicity scales for alpha-proteins (Cid et al., 1992)\cr
Normalized hydrophobicity scales for beta-proteins (Cid et al., 1992)\cr
Normalized hydrophobicity scales for alpha+beta-proteins (Cid et al., 1992)\cr
Normalized hydrophobicity scales for alpha/beta-proteins (Cid et al., 1992)\cr
Normalized average hydrophobicity scales (Cid et al., 1992)\cr
Partial specific volume (Cohn-Edsall, 1943)\cr
Normalized frequency of middle helix (Crawford et al., 1973)\cr
Normalized frequency of beta-sheet (Crawford et al., 1973)\cr
Normalized frequency of turn (Crawford et al., 1973)\cr
Size (Dawson, 1972)\cr
Amino acid composition (Dayhoff et al., 1978a)\cr
Relative mutability (Dayhoff et al., 1978b)\cr
Membrane preference for cytochrome b: MPH89 (Degli Esposti et al., 1990)\cr
Average membrane preference: AMP07 (Degli Esposti et al., 1990)\cr
Consensus normalized hydrophobicity scale (Eisenberg, 1984)\cr
Solvation free energy (Eisenberg-McLachlan, 1986)\cr
Atom-based hydrophobic moment (Eisenberg-McLachlan, 1986)\cr
Direction of hydrophobic moment (Eisenberg-McLachlan, 1986)\cr
Molecular weight (Fasman, 1976)\cr
Melting point (Fasman, 1976)\cr
Optical rotation (Fasman, 1976)\cr
pK-N (Fasman, 1976)\cr
pK-C (Fasman, 1976)\cr
Hydrophobic parameter pi (Fauchere-Pliska, 1983)\cr
Graph shape index (Fauchere et al., 1988)\cr
Smoothed upsilon steric parameter (Fauchere et al., 1988)\cr
Normalized van der Waals volume (Fauchere et al., 1988)\cr
STERIMOL length of the side chain (Fauchere et al., 1988)\cr
STERIMOL minimum width of the side chain (Fauchere et al., 1988)\cr
STERIMOL maximum width of the side chain (Fauchere et al., 1988)\cr
N.m.r. chemical shift of alpha-carbon (Fauchere et al., 1988)\cr
Localized electrical effect (Fauchere et al., 1988)\cr
Number of hydrogen bond donors (Fauchere et al., 1988)\cr
Number of full nonbonding orbitals (Fauchere et al., 1988)\cr
Positive charge (Fauchere et al., 1988)\cr
Negative charge (Fauchere et al., 1988)\cr
pK-a(RCOOH) (Fauchere et al., 1988)\cr
Helix-coil equilibrium constant (Finkelstein-Ptitsyn, 1977)\cr
Helix initiation parameter at posision i-1 (Finkelstein et al., 1991)\cr
Helix initiation parameter at posision i,i+1,i+2 (Finkelstein et al., 1991)\cr
Helix termination parameter at posision j-2,j-1,j (Finkelstein et al., 1991)\cr
Helix termination parameter at posision j+1 (Finkelstein et al., 1991)\cr
Partition coefficient (Garel et al., 1973)\cr
Alpha-helix indices (Geisow-Roberts, 1980)\cr
Alpha-helix indices for alpha-proteins (Geisow-Roberts, 1980)\cr
Alpha-helix indices for beta-proteins (Geisow-Roberts, 1980)\cr
Alpha-helix indices for alpha/beta-proteins (Geisow-Roberts, 1980)\cr
Beta-strand indices (Geisow-Roberts, 1980)\cr
Beta-strand indices for beta-proteins (Geisow-Roberts, 1980)\cr
Beta-strand indices for alpha/beta-proteins (Geisow-Roberts, 1980)\cr
Aperiodic indices (Geisow-Roberts, 1980)\cr
Aperiodic indices for alpha-proteins (Geisow-Roberts, 1980)\cr
Aperiodic indices for beta-proteins (Geisow-Roberts, 1980)\cr
Aperiodic indices for alpha/beta-proteins (Geisow-Roberts, 1980)\cr
Hydrophobicity factor (Goldsack-Chalifoux, 1973)\cr
Residue volume (Goldsack-Chalifoux, 1973)\cr
Composition (Grantham, 1974)\cr
Polarity (Grantham, 1974)\cr
Volume (Grantham, 1974)\cr
Partition energy (Guy, 1985)\cr
Hydration number (Hopfinger, 1971), Cited by Charton-Charton (1982)\cr
Hydrophilicity value (Hopp-Woods, 1981)\cr
Heat capacity (Hutchens, 1970)\cr
Absolute entropy (Hutchens, 1970)\cr
Entropy of formation (Hutchens, 1970)\cr
Normalized relative frequency of alpha-helix (Isogai et al., 1980)\cr
Normalized relative frequency of extended structure (Isogai et al., 1980)\cr
Normalized relative frequency of bend (Isogai et al., 1980)\cr
Normalized relative frequency of bend R (Isogai et al., 1980)\cr
Normalized relative frequency of bend S (Isogai et al., 1980)\cr
Normalized relative frequency of helix end (Isogai et al., 1980)\cr
Normalized relative frequency of double bend (Isogai et al., 1980)\cr
Normalized relative frequency of coil (Isogai et al., 1980)\cr
Average accessible surface area (Janin et al., 1978)\cr
Percentage of buried residues (Janin et al., 1978)\cr
Percentage of exposed residues (Janin et al., 1978)\cr
Ratio of buried and accessible molar fractions (Janin, 1979)\cr
Transfer free energy (Janin, 1979)\cr
Hydrophobicity (Jones, 1975)\cr
pK (-COOH) (Jones, 1975)\cr
Relative frequency of occurrence (Jones et al., 1992)\cr
Relative mutability (Jones et al., 1992)\cr
Amino acid distribution (Jukes et al., 1975)\cr
Sequence frequency (Jungck, 1978)\cr
Average relative probability of helix (Kanehisa-Tsong, 1980)\cr
Average relative probability of beta-sheet (Kanehisa-Tsong, 1980)\cr
Average relative probability of inner helix (Kanehisa-Tsong, 1980)\cr
Average relative probability of inner beta-sheet (Kanehisa-Tsong, 1980)\cr
Flexibility parameter for no rigid neighbors (Karplus-Schulz, 1985)\cr
Flexibility parameter for one rigid neighbor (Karplus-Schulz, 1985)\cr
Flexibility parameter for two rigid neighbors (Karplus-Schulz, 1985)\cr
The Kerr-constant increments (Khanarian-Moore, 1980)\cr
Net charge (Klein et al., 1984)\cr
Side chain interaction parameter (Krigbaum-Rubin, 1971)\cr
Side chain interaction parameter (Krigbaum-Komoriya, 1979)\cr
Fraction of site occupied by water (Krigbaum-Komoriya, 1979)\cr
Side chain volume (Krigbaum-Komoriya, 1979)\cr
Hydropathy index (Kyte-Doolittle, 1982)\cr
Transfer free energy, CHP/water (Lawson et al., 1984)\cr
Hydrophobic parameter (Levitt, 1976)\cr
Distance between C-alpha and centroid of side chain (Levitt, 1976)\cr
Side chain angle theta(AAR) (Levitt, 1976)\cr
Side chain torsion angle phi(AAAR) (Levitt, 1976)\cr
Radius of gyration of side chain (Levitt, 1976)\cr
van der Waals parameter R0 (Levitt, 1976)\cr
van der Waals parameter epsilon (Levitt, 1976)\cr
Normalized frequency of alpha-helix, with weights (Levitt, 1978)\cr
Normalized frequency of beta-sheet, with weights (Levitt, 1978)\cr
Normalized frequency of reverse turn, with weights (Levitt, 1978)\cr
Normalized frequency of alpha-helix, unweighted (Levitt, 1978)\cr
Normalized frequency of beta-sheet, unweighted (Levitt, 1978)\cr
Normalized frequency of reverse turn, unweighted (Levitt, 1978)\cr
Frequency of occurrence in beta-bends (Lewis et al., 1971)\cr
Conformational preference for all beta-strands (Lifson-Sander, 1979)\cr
Conformational preference for parallel beta-strands (Lifson-Sander, 1979)\cr
Conformational preference for antiparallel beta-strands (Lifson-Sander, 1979)\cr
Average surrounding hydrophobicity (Manavalan-Ponnuswamy, 1978)\cr
Normalized frequency of alpha-helix (Maxfield-Scheraga, 1976)\cr
Normalized frequency of extended structure (Maxfield-Scheraga, 1976)\cr
Normalized frequency of zeta R (Maxfield-Scheraga, 1976)\cr
Normalized frequency of left-handed alpha-helix (Maxfield-Scheraga, 1976)\cr
Normalized frequency of zeta L (Maxfield-Scheraga, 1976)\cr
Normalized frequency of alpha region (Maxfield-Scheraga, 1976)\cr
Refractivity (McMeekin et al., 1964), Cited by Jones (1975)\cr
Retention coefficient in HPLC, pH7.4 (Meek, 1980)\cr
Retention coefficient in HPLC, pH2.1 (Meek, 1980)\cr
Retention coefficient in NaClO4 (Meek-Rossetti, 1981)\cr
Retention coefficient in NaH2PO4 (Meek-Rossetti, 1981)\cr
Average reduced distance for C-alpha (Meirovitch et al., 1980)\cr
Average reduced distance for side chain (Meirovitch et al., 1980)\cr
Average side chain orientation angle (Meirovitch et al., 1980)\cr
Effective partition energy (Miyazawa-Jernigan, 1985)\cr
Normalized frequency of alpha-helix (Nagano, 1973)\cr
Normalized frequency of bata-structure (Nagano, 1973)\cr
Normalized frequency of coil (Nagano, 1973)\cr
AA composition of total proteins (Nakashima et al., 1990)\cr
SD of AA composition of total proteins (Nakashima et al., 1990)\cr
AA composition of mt-proteins (Nakashima et al., 1990)\cr
Normalized composition of mt-proteins (Nakashima et al., 1990)\cr
AA composition of mt-proteins from animal (Nakashima et al., 1990)\cr
Normalized composition from animal (Nakashima et al., 1990)\cr
AA composition of mt-proteins from fungi and plant (Nakashima et al., 1990)\cr
Normalized composition from fungi and plant (Nakashima et al., 1990)\cr
AA composition of membrane proteins (Nakashima et al., 1990)\cr
Normalized composition of membrane proteins (Nakashima et al., 1990)\cr
Transmembrane regions of non-mt-proteins (Nakashima et al., 1990)\cr
Transmembrane regions of mt-proteins (Nakashima et al., 1990)\cr
Ratio of average and computed composition (Nakashima et al., 1990)\cr
AA composition of CYT of single-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of CYT2 of single-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of EXT of single-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of EXT2 of single-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of MEM of single-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of CYT of multi-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of EXT of multi-spanning proteins (Nakashima-Nishikawa, 1992)\cr
AA composition of MEM of multi-spanning proteins (Nakashima-Nishikawa, 1992)\cr
8 A contact number (Nishikawa-Ooi, 1980)\cr
14 A contact number (Nishikawa-Ooi, 1986)\cr
Transfer energy, organic solvent/water (Nozaki-Tanford, 1971)\cr
Average non-bonded energy per atom (Oobatake-Ooi, 1977)\cr
Short and medium range non-bonded energy per atom (Oobatake-Ooi, 1977)\cr
Long range non-bonded energy per atom (Oobatake-Ooi, 1977)\cr
Average non-bonded energy per residue (Oobatake-Ooi, 1977)\cr
Short and medium range non-bonded energy per residue (Oobatake-Ooi, 1977)\cr
Optimized beta-structure-coil equilibrium constant (Oobatake et al., 1985)\cr
Optimized propensity to form reverse turn (Oobatake et al., 1985)\cr
Optimized transfer energy parameter (Oobatake et al., 1985)\cr
Optimized average non-bonded energy per atom (Oobatake et al., 1985)\cr
Optimized side chain interaction parameter (Oobatake et al., 1985)\cr
Normalized frequency of alpha-helix from LG (Palau et al., 1981)\cr
Normalized frequency of alpha-helix from CF (Palau et al., 1981)\cr
Normalized frequency of beta-sheet from LG (Palau et al., 1981)\cr
Normalized frequency of beta-sheet from CF (Palau et al., 1981)\cr
Normalized frequency of turn from LG (Palau et al., 1981)\cr
Normalized frequency of turn from CF (Palau et al., 1981)\cr
Normalized frequency of alpha-helix in all-alpha class (Palau et al., 1981)\cr
Normalized frequency of alpha-helix in alpha+beta class (Palau et al., 1981)\cr
Normalized frequency of alpha-helix in alpha/beta class (Palau et al., 1981)\cr
Normalized frequency of beta-sheet in all-beta class (Palau et al., 1981)\cr
Normalized frequency of beta-sheet in alpha+beta class (Palau et al., 1981)\cr
Normalized frequency of beta-sheet in alpha/beta class (Palau et al., 1981)\cr
Normalized frequency of turn in all-alpha class (Palau et al., 1981)\cr
Normalized frequency of turn in all-beta class (Palau et al., 1981)\cr
Normalized frequency of turn in alpha+beta class (Palau et al., 1981)\cr
Normalized frequency of turn in alpha/beta class (Palau et al., 1981)\cr
HPLC parameter (Parker et al., 1986)\cr
Partition coefficient (Pliska et al., 1981)\cr
Surrounding hydrophobicity in folded form (Ponnuswamy et al., 1980)\cr
Average gain in surrounding hydrophobicity (Ponnuswamy et al., 1980)\cr
Average gain ratio in surrounding hydrophobicity (Ponnuswamy et al., 1980)\cr
Surrounding hydrophobicity in alpha-helix (Ponnuswamy et al., 1980)\cr
Surrounding hydrophobicity in beta-sheet (Ponnuswamy et al., 1980)\cr
Surrounding hydrophobicity in turn (Ponnuswamy et al., 1980)\cr
Accessibility reduction ratio (Ponnuswamy et al., 1980)\cr
Average number of surrounding residues (Ponnuswamy et al., 1980)\cr
Intercept in regression analysis (Prabhakaran-Ponnuswamy, 1982)\cr
Slope in regression analysis x 1.0E1 (Prabhakaran-Ponnuswamy, 1982)\cr
Correlation coefficient in regression analysis (Prabhakaran-Ponnuswamy, 1982)\cr
Hydrophobicity (Prabhakaran, 1990)\cr
Relative frequency in alpha-helix (Prabhakaran, 1990)\cr
Relative frequency in beta-sheet (Prabhakaran, 1990)\cr
Relative frequency in reverse-turn (Prabhakaran, 1990)\cr
Helix-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)\cr
Beta-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)\cr
Weights for alpha-helix at the window position of -6 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of -5 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of -4 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of -3 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of -2 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of -1 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 0 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 1 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 2 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 3 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 4 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 5 (Qian-Sejnowski, 1988)\cr
Weights for alpha-helix at the window position of 6 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of -6 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of -5 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of -4 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of -3 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of -2 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of -1 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 0 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 1 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 2 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 3 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 4 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 5 (Qian-Sejnowski, 1988)\cr
Weights for beta-sheet at the window position of 6 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of -6 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of -5 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of -4 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of -3 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of -2 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of -1 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 0 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 1 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 2 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 3 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 4 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 5 (Qian-Sejnowski, 1988)\cr
Weights for coil at the window position of 6 (Qian-Sejnowski, 1988)\cr
Average reduced distance for C-alpha (Rackovsky-Scheraga, 1977)\cr
Average reduced distance for side chain (Rackovsky-Scheraga, 1977)\cr
Side chain orientational preference (Rackovsky-Scheraga, 1977)\cr
Average relative fractional occurrence in A0(i) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in AR(i) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in AL(i) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in EL(i) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in E0(i) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in ER(i) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in A0(i-1) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in AR(i-1) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in AL(i-1) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in EL(i-1) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in E0(i-1) (Rackovsky-Scheraga, 1982)\cr
Average relative fractional occurrence in ER(i-1) (Rackovsky-Scheraga, 1982)\cr
Value of theta(i) (Rackovsky-Scheraga, 1982)\cr
Value of theta(i-1) (Rackovsky-Scheraga, 1982)\cr
Transfer free energy from chx to wat (Radzicka-Wolfenden, 1988)\cr
Transfer free energy from oct to wat (Radzicka-Wolfenden, 1988)\cr
Transfer free energy from vap to chx (Radzicka-Wolfenden, 1988)\cr
Transfer free energy from chx to oct (Radzicka-Wolfenden, 1988)\cr
Transfer free energy from vap to oct (Radzicka-Wolfenden, 1988)\cr
Accessible surface area (Radzicka-Wolfenden, 1988)\cr
Energy transfer from out to in(95%buried) (Radzicka-Wolfenden, 1988)\cr
Mean polarity (Radzicka-Wolfenden, 1988)\cr
Relative preference value at N" (Richardson-Richardson, 1988)\cr
Relative preference value at N' (Richardson-Richardson, 1988)\cr
Relative preference value at N-cap (Richardson-Richardson, 1988)\cr
Relative preference value at N1 (Richardson-Richardson, 1988)\cr
Relative preference value at N2 (Richardson-Richardson, 1988)\cr
Relative preference value at N3 (Richardson-Richardson, 1988)\cr
Relative preference value at N4 (Richardson-Richardson, 1988)\cr
Relative preference value at N5 (Richardson-Richardson, 1988)\cr
Relative preference value at Mid (Richardson-Richardson, 1988)\cr
Relative preference value at C5 (Richardson-Richardson, 1988)\cr
Relative preference value at C4 (Richardson-Richardson, 1988)\cr
Relative preference value at C3 (Richardson-Richardson, 1988)\cr
Relative preference value at C2 (Richardson-Richardson, 1988)\cr
Relative preference value at C1 (Richardson-Richardson, 1988)\cr
Relative preference value at C-cap (Richardson-Richardson, 1988)\cr
Relative preference value at C' (Richardson-Richardson, 1988)\cr
Relative preference value at C" (Richardson-Richardson, 1988)\cr
Information measure for alpha-helix (Robson-Suzuki, 1976)\cr
Information measure for N-terminal helix (Robson-Suzuki, 1976)\cr
Information measure for middle helix (Robson-Suzuki, 1976)\cr
Information measure for C-terminal helix (Robson-Suzuki, 1976)\cr
Information measure for extended (Robson-Suzuki, 1976)\cr
Information measure for pleated-sheet (Robson-Suzuki, 1976)\cr
Information measure for extended without H-bond (Robson-Suzuki, 1976)\cr
Information measure for turn (Robson-Suzuki, 1976)\cr
Information measure for N-terminal turn (Robson-Suzuki, 1976)\cr
Information measure for middle turn (Robson-Suzuki, 1976)\cr
Information measure for C-terminal turn (Robson-Suzuki, 1976)\cr
Information measure for coil (Robson-Suzuki, 1976)\cr
Information measure for loop (Robson-Suzuki, 1976)\cr
Hydration free energy (Robson-Osguthorpe, 1979)\cr
Mean area buried on transfer (Rose et al., 1985)\cr
Mean fractional area loss (Rose et al., 1985)\cr
Side chain hydropathy, uncorrected for solvation (Roseman, 1988)\cr
Side chain hydropathy, corrected for solvation (Roseman, 1988)\cr
Loss of Side chain hydropathy by helix formation (Roseman, 1988)\cr
Transfer free energy (Simon, 1976), Cited by Charton-Charton (1982)\cr
Principal component I (Sneath, 1966)\cr
Principal component II (Sneath, 1966)\cr
Principal component III (Sneath, 1966)\cr
Principal component IV (Sneath, 1966)\cr
Zimm-Bragg parameter s at 20 C (Sueki et al., 1984)\cr
Zimm-Bragg parameter sigma x 1.0E4 (Sueki et al., 1984)\cr
Optimal matching hydrophobicity (Sweet-Eisenberg, 1983)\cr
Normalized frequency of alpha-helix (Tanaka-Scheraga, 1977)\cr
Normalized frequency of isolated helix (Tanaka-Scheraga, 1977)\cr
Normalized frequency of extended structure (Tanaka-Scheraga, 1977)\cr
Normalized frequency of chain reversal R (Tanaka-Scheraga, 1977)\cr
Normalized frequency of chain reversal S (Tanaka-Scheraga, 1977)\cr
Normalized frequency of chain reversal D (Tanaka-Scheraga, 1977)\cr
Normalized frequency of left-handed helix (Tanaka-Scheraga, 1977)\cr
Normalized frequency of zeta R (Tanaka-Scheraga, 1977)\cr
Normalized frequency of coil (Tanaka-Scheraga, 1977)\cr
Normalized frequency of chain reversal (Tanaka-Scheraga, 1977)\cr
Relative population of conformational state A (Vasquez et al., 1983)\cr
Relative population of conformational state C (Vasquez et al., 1983)\cr
Relative population of conformational state E (Vasquez et al., 1983)\cr
Electron-ion interaction potential (Veljkovic et al., 1985)\cr
Bitterness (Venanzi, 1984)\cr
Transfer free energy to lipophilic phase (von Heijne-Blomberg, 1979)\cr
Average interactions per side chain atom (Warme-Morgan, 1978)\cr
RF value in high salt chromatography (Weber-Lacey, 1978)\cr
Propensity to be buried inside (Wertz-Scheraga, 1978)\cr
Free energy change of epsilon(i) to epsilon(ex) (Wertz-Scheraga, 1978)\cr
Free energy change of alpha(Ri) to alpha(Rh) (Wertz-Scheraga, 1978)\cr
Free energy change of epsilon(i) to alpha(Rh) (Wertz-Scheraga, 1978)\cr
Polar requirement (Woese, 1973)\cr
Hydration potential (Wolfenden et al., 1981)\cr
Principal property value z1 (Wold et al., 1987)\cr
Principal property value z2 (Wold et al., 1987)\cr
Principal property value z3 (Wold et al., 1987)\cr
Unfolding Gibbs energy in water, pH7.0 (Yutani et al., 1987)\cr
Unfolding Gibbs energy in water, pH9.0 (Yutani et al., 1987)\cr
Activation Gibbs energy of unfolding, pH7.0 (Yutani et al., 1987)\cr
Activation Gibbs energy of unfolding, pH9.0 (Yutani et al., 1987)\cr
Dependence of partition coefficient on ionic strength (Zaslavsky et al., 1982)\cr
Hydrophobicity (Zimmerman et al., 1968)\cr
Bulkiness (Zimmerman et al., 1968)\cr
Polarity (Zimmerman et al., 1968)\cr
Isoelectric point (Zimmerman et al., 1968)\cr
RF rank (Zimmerman et al., 1968)\cr
Normalized positional residue frequency at helix termini N4'(Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N"' (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N" (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N'(Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini Nc (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N1 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N2 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N3 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N4 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini N5 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C5 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C4 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C3 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C2 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C1 (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini Cc (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C' (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C" (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C"' (Aurora-Rose, 1998)\cr
Normalized positional residue frequency at helix termini C4' (Aurora-Rose, 1998)\cr
Delta G values for the peptides extrapolated to 0 M urea (O'Neil-DeGrado, 1990)\cr
Helix formation parameters (delta delta G) (O'Neil-DeGrado, 1990)\cr
Normalized flexibility parameters (B-values), average (Vihinen et al., 1994)\cr
Normalized flexibility parameters (B-values) for each residue surrounded by none rigid neighbours (Vihinen et al., 1994)\cr
Normalized flexibility parameters (B-values) for each residue surrounded by one rigid neighbours (Vihinen et al., 1994)\cr
Normalized flexibility parameters (B-values) for each residue surrounded by two rigid neighbours (Vihinen et al., 1994)\cr
Free energy in alpha-helical conformation (Munoz-Serrano, 1994)\cr
Free energy in alpha-helical region (Munoz-Serrano, 1994)\cr
Free energy in beta-strand conformation (Munoz-Serrano, 1994)\cr
Free energy in beta-strand region (Munoz-Serrano, 1994)\cr
Free energy in beta-strand region (Munoz-Serrano, 1994)\cr
Free energies of transfer of AcWl-X-LL peptides from bilayer interface to water (Wimley-White, 1996)\cr
Thermodynamic beta sheet propensity (Kim-Berg, 1993)\cr
Turn propensity scale for transmembrane helices (Monne et al., 1999)\cr
Alpha helix propensity of position 44 in T4 lysozyme (Blaber et al., 1993)\cr
p-Values of mesophilic proteins based on the distributions of B values (Parthasarathy-Murthy, 2000)\cr
p-Values of thermophilic proteins based on the distributions of B values (Parthasarathy-Murthy, 2000)\cr
Distribution of amino acid residues in the 18 non-redundant families of thermophilic proteins (Kumar et al., 2000)\cr
Distribution of amino acid residues in the 18 non-redundant families of mesophilic proteins (Kumar et al., 2000)\cr
Distribution of amino acid residues in the alpha-helices in thermophilic proteins (Kumar et al., 2000)\cr
Distribution of amino acid residues in the alpha-helices in mesophilic proteins (Kumar et al., 2000)\cr
Side-chain contribution to protein stability (kJ/mol) (Takano-Yutani, 2001)\cr
Propensity of amino acids within pi-helices (Fodje-Al-Karadaghi, 2002)\cr
Hydropathy scale based on self-information values in the two-state model (5% accessibility) (Naderi-Manesh et al., 2001)\cr
Hydropathy scale based on self-information values in the two-state model (9% accessibility) (Naderi-Manesh et al., 2001)\cr
Hydropathy scale based on self-information values in the two-state model (16% accessibility) (Naderi-Manesh et al., 2001)\cr
Hydropathy scale based on self-information values in the two-state model (20% accessibility) (Naderi-Manesh et al., 2001)\cr
Hydropathy scale based on self-information values in the two-state model (25% accessibility) (Naderi-Manesh et al., 2001)\cr
Hydropathy scale based on self-information values in the two-state model (36% accessibility) (Naderi-Manesh et al., 2001)\cr
Hydropathy scale based on self-information values in the two-state model (50% accessibility) (Naderi-Manesh et al., 2001)\cr
Averaged turn propensities in a transmembrane helix (Monne et al., 1999)\cr
Alpha-helix propensity derived from designed sequences (Koehl-Levitt, 1999)\cr
Beta-sheet propensity derived from designed sequences (Koehl-Levitt, 1999)\cr
Composition of amino acids in extracellular proteins (percent) (Cedano et al., 1997)\cr
Composition of amino acids in anchored proteins (percent) (Cedano et al., 1997)\cr
Composition of amino acids in membrane proteins (percent) (Cedano et al., 1997)\cr
Composition of amino acids in intracellular proteins (percent) (Cedano et al., 1997)\cr
Composition of amino acids in nuclear proteins (percent) (Cedano et al., 1997)\cr
Surface composition of amino acids in intracellular proteins of thermophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Surface composition of amino acids in intracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Surface composition of amino acids in extracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Surface composition of amino acids in nuclear proteins (percent) (Fukuchi-Nishikawa, 2001)\cr
Interior composition of amino acids in intracellular proteins of thermophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Interior composition of amino acids in intracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Interior composition of amino acids in extracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Interior composition of amino acids in nuclear proteins (percent) (Fukuchi-Nishikawa, 2001)\cr
Entire chain composition of amino acids in intracellular proteins of thermophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Entire chain composition of amino acids in intracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Entire chain composition of amino acids in extracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)\cr
Entire chain compositino of amino acids in nuclear proteins (percent) (Fukuchi-Nishikawa, 2001)\cr
Screening coefficients gamma, local (Avbelj, 2000)\cr
Screening coefficients gamma, non-local (Avbelj, 2000)\cr
Slopes tripeptide, FDPB VFF neutral (Avbelj, 2000)\cr
Slopes tripeptides, LD VFF neutral (Avbelj, 2000)\cr
Slopes tripeptide, FDPB VFF noside (Avbelj, 2000)\cr
Slopes tripeptide FDPB VFF all (Avbelj, 2000)\cr
Slopes tripeptide FDPB PARSE neutral (Avbelj, 2000)\cr
Slopes dekapeptide, FDPB VFF neutral (Avbelj, 2000)\cr
Slopes proteins, FDPB VFF neutral (Avbelj, 2000)\cr
Side-chain conformation by gaussian evolutionary method (Yang et al., 2002)\cr
Amphiphilicity index (Mitaku et al., 2002)\cr
Volumes including the crystallographic waters using the ProtOr (Tsai et al., 1999)\cr
Volumes not including the crystallographic waters using the ProtOr (Tsai et al., 1999)\cr
Electron-ion interaction potential values (Cosic, 1994)\cr
Hydrophobicity scales (Ponnuswamy, 1993)\cr
Hydrophobicity coefficient in RP-HPLC, C18 with 0.1%TFA/MeCN/H2O (Wilce et al. 1995)\cr
Hydrophobicity coefficient in RP-HPLC, C8 with 0.1%TFA/MeCN/H2O (Wilce et al. 1995)\cr
Hydrophobicity coefficient in RP-HPLC, C4 with 0.1%TFA/MeCN/H2O (Wilce et al. 1995)\cr
Hydrophobicity coefficient in RP-HPLC, C18 with 0.1%TFA/2-PrOH/MeCN/H2O (Wilce et al. 1995)\cr
Hydrophilicity scale (Kuhn et al., 1995)\cr
Retention coefficient at pH 2 (Guo et al., 1986)\cr
Modified Kyte-Doolittle hydrophobicity scale (Juretic et al., 1998)\cr
Interactivity scale obtained from the contact matrix (Bastolla et al., 2005)\cr
Interactivity scale obtained by maximizing the mean of correlation coefficient over single-domain globular proteins (Bastolla et al., 2005)\cr
Interactivity scale obtained by maximizing the mean of correlation coefficient over pairs of sequences sharing the TIM barrel fold (Bastolla et al., 2005)\cr
Linker propensity index (Suyama-Ohara, 2003)\cr
Knowledge-based membrane-propensity scale from 1D\_Helix in MPtopo databases (Punta-Maritan, 2003)\cr
Knowledge-based membrane-propensity scale from 3D\_Helix in MPtopo databases (Punta-Maritan, 2003)\cr
Linker propensity from all dataset (George-Heringa, 2003)\cr
Linker propensity from 1-linker dataset (George-Heringa, 2003)\cr
Linker propensity from 2-linker dataset (George-Heringa, 2003)\cr
Linker propensity from 3-linker dataset (George-Heringa, 2003)\cr
Linker propensity from small dataset (linker length is less than six residues) (George-Heringa, 2003)\cr
Linker propensity from medium dataset (linker length is between six and 14 residues) (George-Heringa, 2003)\cr
Linker propensity from long dataset (linker length is greater than 14 residues) (George-Heringa, 2003)\cr
Linker propensity from helical (annotated by DSSP) dataset (George-Heringa, 2003)\cr
Linker propensity from non-helical (annotated by DSSP) dataset (George-Heringa, 2003)\cr
The stability scale from the knowledge-based atom-atom potential (Zhou-Zhou, 2004)\cr
The relative stability scale extracted from mutation experiments (Zhou-Zhou, 2004)\cr
Buriability (Zhou-Zhou, 2004)\cr
Linker index (Bae et al., 2005)\cr
Mean volumes of residues buried in protein interiors (Harpaz et al., 1994)\cr
Average volumes of residues (Pontius et al., 1996)\cr
Hydrostatic pressure asymmetry index, PAI (Di Giulio, 2005)\cr
Hydrophobicity index (Wolfenden et al., 1979)\cr
Average internal preferences (Olsen, 1980)\cr
Hydrophobicity-related index (Kidera et al., 1985)\cr
Apparent partition energies calculated from Wertz-Scheraga index (Guy, 1985)\cr
Apparent partition energies calculated from Robson-Osguthorpe index (Guy, 1985)\cr
Apparent partition energies calculated from Janin index (Guy, 1985)\cr
Apparent partition energies calculated from Chothia index (Guy, 1985)\cr
Hydropathies of amino acid side chains, neutral form (Roseman, 1988)\cr
Hydropathies of amino acid side chains, pi-values in pH 7.0 (Roseman, 1988)\cr
Weights from the IFH scale (Jacobs-White, 1989)\cr
Hydrophobicity index, 3.0 pH (Cowan-Whittaker, 1990)\cr
Scaled side chain hydrophobicity values (Black-Mould, 1991)\cr
Hydrophobicity scale from native protein structures (Casari-Sippl, 1992)\cr
NNEIG index (Cornette et al., 1987)\cr
SWEIG index (Cornette et al., 1987)\cr
PRIFT index (Cornette et al., 1987)\cr
PRILS index (Cornette et al., 1987)\cr
ALTFT index (Cornette et al., 1987)\cr
ALTLS index (Cornette et al., 1987)\cr
TOTFT index (Cornette et al., 1987)\cr
TOTLS index (Cornette et al., 1987)\cr
Relative partition energies derived by the Bethe approximation (Miyazawa-Jernigan, 1999)\cr
Optimized relative partition energies - method A (Miyazawa-Jernigan, 1999)\cr
Optimized relative partition energies - method B (Miyazawa-Jernigan, 1999)\cr
Optimized relative partition energies - method C (Miyazawa-Jernigan, 1999)\cr
Optimized relative partition energies - method D (Miyazawa-Jernigan, 1999)\cr
Hydrophobicity index (Engelman et al., 1986)\cr
Hydrophobicity index (Fasman, 1989)
}
\source{
\url{https://www.genome.jp/aaindex/}
}
\references{

From the original aaindex documentation:\cr

Please cite the following references when making use of the database:

Kawashima, S. and Kanehisa, M. (2000) AAindex: amino acid index 
database. \emph{Nucleic Acids Res.}, \bold{28}:374.\cr

Tomii, K. and Kanehisa, M. (1996) Analysis of amino acid indices and
mutation matrices for sequence comparison and structure
prediction of proteins. \emph{Protein Eng.}, \bold{9}:27-36.\cr

Nakai, K., Kidera, A., and Kanehisa, M. (1988) Cluster analysis of
amino acid indices for prediction of protein structure and
function. \emph{Protein Eng.} \bold{2}:93-100.\cr

}
\examples{
#
# Load data:
#

data(aaindex)

#
# Supose that we need the Kyte & Doolittle Hydrophaty index. We first look
# at the entries with Kyte as author:
#

which(sapply(aaindex, function(x) length(grep("Kyte", x$A)) != 0))

#
# This should return that entry number 151 named KYTJ820101 is the only
# one that fit our request. We can access to it by position or by name,
# for instance:
#

aaindex[[151]]$I
aaindex[["KYTJ820101"]]$I
aaindex$KYTJ820101$I

}
\keyword{datasets}
