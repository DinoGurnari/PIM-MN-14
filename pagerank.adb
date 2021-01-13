with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_line; use Ada.Command_line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Algebre;

procedure pagerank is

	Reseau : Ada.Text_IO.File_Type;
	Nom_Reseau : Unbounded_String := To_Unbounded_String("exemple_sujet.net");
	alpha : float := 0.85;
	Iteration : Integer := 150;
	K : Integer := 1;
	Mode : Character := 'C';
	Arg : Unbounded_String;


	-- On récupère le nombre N de pages du reseau
	function Determiner_Taille(Nom_Reseau : String) return Integer is
		Reseau : Ada.Text_IO.File_Type;
		N : Integer;
	begin
		open(Reseau, In_File, Nom_Reseau);
		get(Reseau, N);
		close(Reseau);
		return(N);
	end Determiner_Taille;

	N : Integer := Determiner_Taille(To_String(Nom_Reseau));
	-- On utlise des matrices de taille N*N
	package AlgebreN is new Algebre(N);
	use AlgebreN;

	-- Calcul de la matrice de Google Naive
	function Matrice_Google_Naive(Nom_Reseau : String) return T_Matrice is
		M : T_Matrice;
		Attila : T_Matrice;

		-- On forme la matrice S
		procedure Creer_S(M : in out T_Matrice; Nom_Reseau : String) is
			Reseau : Ada.Text_IO.File_Type;
			Site1 : Integer;
			Site2 : Integer;
		begin
			-- Créer H à partir du reseau donnée
			open(Reseau, In_File, Nom_Reseau);
			Skip_Line(Reseau);
			-- Créer H contenant des ‘1’ pour représenter les hyperliens
			while not End_Of_File(Reseau) loop
				-- Traiter l’information de la ligne i
				Get(Reseau, Site1);
				Get(Reseau, Site2);
				M(Site1 + 1)(Site2 + 1) := 1.0;
			end loop;
			close(Reseau);

			-- Créer S à l’aide de H
			for i in 1..N loop
				-- Vérifier qu’une ligne de H est vide
				if Ligne_Vide(M, i) then
					-- Remplacer les lignes vides de S par des lignes de 1/N
					Initialiser_Vecteur(M(i), 1.0/float(N));
				else
					-- Normaliser les lignes de H
					Normaliser_Ligne(M, i);
				end if;
			end loop;

		end Creer_S;

	begin
		Initialiser_Matrice(M, 0.0);
		Initialiser_Matrice(Attila, 1.0);
		Creer_S(M, Nom_Reseau);
		-- Calculer G en fonction de S
		Produit_Scalaire_Matrice(alpha, M);
		Produit_Scalaire_Matrice((1.0 - alpha)/float(N), Attila);
		M := Somme_Matrice(M, Attila);
		return(M);
	end Matrice_Google_Naive;

	G : T_Matrice;
	Poids : T_Vecteur;

	-- Calcul du PageRank pour un certain nombre d'itération
	procedure Calcul_PageRank(Pi : in out T_Vecteur; Iteration : in Integer) is
	begin
		Initialiser_Vecteur(Pi, 1.0/float(N));
		for k in 1..Iteration loop
			-- Calculer Pi(k+1) en fonction de G
			Pi := Produit_Vecteur_Matrice(Pi, G);
		end loop;
	end Calcul_PageRank;

begin

	while K < Argument_Count loop
		Arg := To_Unbounded_String(Argument(K));

		if Arg = "-P" then
			-- Mode Naif
			Mode := 'N';
			K := K + 1;

		elsif Arg = "-I" then
			-- Choix nbre itération
			Iteration := Integer'Value(Argument(K+1));
			K := K + 2;

		elsif Arg = "-A" then
			-- Choix alpha
			alpha := Float'Value(Argument(K+1));
			K := K + 2;

		end if;
	end loop;

	Nom_Reseau := To_Unbounded_String(Argument(Argument_Count));
	Put(Iteration,1);
	Put(" Iterations");
	New_Line;
	Put(alpha,1);
	Put(" = Alpha");
	New_Line;

	if Mode = 'N' then
		Put_Line("Calcul Naif");
		G := Matrice_Google_Naive(To_String(Nom_Reseau));
		Calcul_PageRank(Poids, Iteration);

	elsif Mode = 'C' then
		Put_Line("Matrices creuses non implementees");

	else
		Put_Line("Erreur dans le choix du mode");
	end if;

end pagerank;
