with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_line; use Ada.Command_line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Algebre;

procedure pagerank is

	Reseau : Ada.Text_IO.File_Type;
	Nom_Reseau : Unbounded_String := To_Unbounded_String(Argument(Argument_Count));
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
	PRank : T_Vecteur;

	-- Calcul du Poids pour un certain nombre d'itération
	procedure Calcul_Poids(Pi : in out T_Vecteur; Iteration : in Integer) is
	begin
		Initialiser_Vecteur(Pi, 1.0/float(N));
		for k in 1..Iteration loop
			-- Calculer Pi(k+1) en fonction de G
			Pi := Produit_Vecteur_Matrice(Pi, G);
		end loop;
	end Calcul_Poids;

	function Creation_Vecteur_Pagerank return T_Vecteur is
		PageRank : T_Vecteur;
	begin
		for i in 1..N loop
			PageRank(i) := float(i - 1);
		end loop;

		return PageRank;
	end;

	-- Creation des fichiers
	procedure Creation_Fichiers(PageRank : in T_Vecteur; Poids : in T_Vecteur) is
		F_PageRank : Ada.Text_IO.File_Type;
		F_Poids : Ada.Text_IO.File_Type;
		nom : Unbounded_String := To_Unbounded_String(Slice(Nom_Reseau, 1, Length(Nom_Reseau) - 3));
		nom_pagerank : Unbounded_String := nom & "ord";
		nom_poids : Unbounded_String := nom & "p";
	begin
		Create(F_PageRank, Out_File, To_String(nom_pagerank));
		Create(F_Poids, Out_File, To_String(nom_poids));

		-- Ajout de la ligne contenant N alpha I
		Put(F_Poids , N, 1);
		Put(F_Poids , " ");
		Put(F_Poids , alpha, Fore => 1, Aft => 10);
		Put(F_Poids , " ");
		Put(F_Poids , Iteration, 1);
		New_Line(F_Poids);

		for l in 1..N loop
			-- On ajoute les lignes;
			Put(F_PageRank, Integer(PageRank(l)), 1);
			New_Line(F_PageRank);
			Put(F_Poids, Poids(l), Fore => 1, Aft => 10);
			New_Line(F_Poids);
		end loop;

		Close(F_Poids);
		Close(F_PageRank);
	end;

begin
	-- Lecture des arguments de la ligne de commandes
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

		else
			Put_Line("Les options possibles sont :");
			New_Line;
			Put_Line("-P pour le mode Naif");
			New_Line;
			Put_Line("-I suivie d'un Entier Naturel pour choisir le nombre d'itération");
			New_Line;
			Put_Line("-A suivie d'un Réel compris entre 0 et 1 pour choisir alpha");
			New_Line;
			Put_Line("Le nom du fichier à traiter doit être mis en dernier !");

		end if;
	end loop;

	Put(Iteration,1);
	Put(" Iterations");
	New_Line;
	Put(alpha,1);
	Put(" = Alpha");
	New_Line;

	if Mode = 'N' then
		Put_Line("Calcul Naif");
		G := Matrice_Google_Naive(To_String(Nom_Reseau));
		Calcul_Poids(Poids, Iteration);
		PRank := Creation_Vecteur_Pagerank;
		Creation_Fichiers(PRank, Poids);

	elsif Mode = 'C' then
		Put_Line("Matrices creuses non implementees");

	else
		Put_Line("Erreur dans le choix du mode");
	end if;

end pagerank;
