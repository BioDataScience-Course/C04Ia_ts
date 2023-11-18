# Vérifications de beer.qmd
beer <- parse_rmd("../../beer.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("beer.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("beer.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes", "Résultats", "Discussion & conclusions")
    %in% (rmd_node_sections(beer) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "import", "bacf", "bacfcomment", "bssl", "bsslcomment", "btrend", "btrendcomment", "bspectrum", "bspectrumcomment", "boxplot", "boxplotcomment")
    %in% rmd_node_label(beer)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(beer))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  expect_true(beer[[1]]$author != "___")
  expect_true(!grepl("__", beer[[1]]$author))
  expect_true(grepl("^[^_]....+", beer[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", beer[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", beer[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import' : Création de l'objet 'beer_ts'", {
  expect_true(is_identical_to_ref("import", "tsp"))
  # La série 'beer_ts' n'est pas correcte. Les valeurs de début, 
  # de fin et/ou de fréquence sont (partiellement) incorrectes.
  
  expect_true(is_identical_to_ref("import", "class"))
  # La série 'beer_ts' n'est pas correcte. Il ne s'agit pas d'un objet 'ts'. 
  # Avez-vous bien obtenu un objet ts et non un objet 'mts' 
})

test_that("Chunks 'bacf' & 'bacfcomment' : Analyse de l'autocorrélation", {
  # expect_true(is_identical_to_ref("bacf"))
  # Le graphique de l'autocorrélation n'est pas réalisé ou est incorrect.
  
  expect_true(is_identical_to_ref("bacfcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'bssl' & 'bsslcomment' : Statistiques glissantes", {
  expect_true(is_identical_to_ref("bssl"))
  # Les statistiques glissantes ne sont pas réalisées ou sont incorrectes.
  # Avez-vous bien employé un pas de temps de 5 ans ? .
  
  expect_true(is_identical_to_ref("bsslcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'btrend' & 'btrendcomment' : Test de tendance générale par bootstrap", {
  expect_true(is_identical_to_ref("btrend"))
  # Le test de tendance générale par bootstrap n'est pas réalisé ou est incorrect.
  # Avez-vous bien employé une valeur de R de 500.
  
  expect_true(is_identical_to_ref("btrendcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'bspectrum' & 'bspectrumcomment' : Périodogramme de la série stationnarisée", {
  expect_true(is_identical_to_ref("bspectrum"))
  # Le graphique n'est pas réalisé ou est incorrect.
  # Avez-vous bien employé les valeurs de 3 et de 5 sur votre série 
  # stationnariée ('beer_stat') ?
  
  expect_true(is_identical_to_ref("bspectrumcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cochez la phrase qui décrit le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
  
})

test_that("Chunks 'boxplot' & 'boxplotcomment' : Boites de dispersion de la série stationnarisée", {
  # expect_true(is_identical_to_ref("boxplot"))
  # Le graphique n'est pas réalisé ou est incorrect.
  # Avez-vous bien employé la série stationnarisée ('beer_stat') ?

  expect_true(is_identical_to_ref("boxplotcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

})

test_that("La partie discussion et conclusions est-elle remplie ?", {
  expect_true(!(rmd_select(beer, by_section("Discussion & conclusions")) |>
      as_document() |> grepl("...votre discussion ici...", x = _,
        fixed = TRUE) |> any()))
  # La discussion et la conclusion ne sont pas faites
  # Remplacez "...votre discussion ici..." par vos phrases de commentaires
  # libres (à noter que le contenu de cette section n'est pas évalué
  # automatiquement, mais il le sera par vos enseignants).
})
