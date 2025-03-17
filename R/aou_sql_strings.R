#' Guess sql query type from raw text
#'
#' @noRd
guess_sql_query_type <- function(x) {
  if (grepl("c_occurrence.", x, fixed = TRUE)) return("conditions")
  if (grepl("survey.", x, fixed = TRUE)) return("surveys")
  if (grepl("measurement.", x, fixed = TRUE)) return("measurements")
  if (grepl("person.", x, fixed = TRUE)) return("person")
  stop("Unknown sql type")
}

# Patients ----------------------------------------------------------------

#' SQL query to create data frame of basic patient info with pneumonia
#'
#' type: person
#' @export
sql_patients <- "
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth,
        person.self_reported_category_concept_id,
        p_self_reported_category_concept.concept_name as self_reported_category
    FROM
        `person` person
    LEFT JOIN
        `concept` p_gender_concept
            ON person.gender_concept_id = p_gender_concept.concept_id
    LEFT JOIN
        `concept` p_race_concept
            ON person.race_concept_id = p_race_concept.concept_id
    LEFT JOIN
        `concept` p_ethnicity_concept
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id
    LEFT JOIN
        `concept` p_sex_at_birth_concept
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id
    LEFT JOIN
        `concept` p_self_reported_category_concept
            ON person.self_reported_category_concept_id = p_self_reported_category_concept.concept_id
    WHERE
        person.PERSON_ID IN (SELECT
            distinct person_id
        FROM
            `cb_search_person` cb_search_person
        WHERE
            cb_search_person.person_id IN (SELECT
                criteria.person_id
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id
                FROM
                    `cb_search_all_events`
                WHERE
                    (concept_id IN(SELECT
                        DISTINCT c.concept_id
                    FROM
                        `cb_criteria` c
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id
                        FROM
                            `cb_criteria` cr
                        WHERE
                            concept_id IN (255848)
                            AND full_text LIKE '%_rank1]%'      ) a
                            ON (c.path LIKE CONCAT('%.', a.id, '.%')
                            OR c.path LIKE CONCAT('%.', a.id)
                            OR c.path LIKE CONCAT(a.id, '.%')
                            OR c.path = a.id)
                    WHERE
                        is_standard = 1
                        AND is_selectable = 1)
                    AND is_standard = 1 )) criteria )
            AND cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_whole_genome_variant = 1
            UNION
            DISTINCT SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_lr_whole_genome_variant = 1
            UNION
            DISTINCT SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_structural_variant_data = 1 )
            AND cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_ehr_data = 1 )
            AND cb_search_person.person_id NOT IN (SELECT
                temp1.person_id
            FROM
                (SELECT
                    person_id, visit_occurrence_id, entry_date
                FROM
                    `cb_search_all_events`
                WHERE
                    concept_id IN(SELECT
                        DISTINCT c.concept_id
                    FROM
                        `cb_criteria` c
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id
                        FROM
                            `cb_criteria` cr
                        WHERE
                            concept_id IN (255848)
                            AND full_text LIKE '%_rank1]%'      ) a
                            ON (c.path LIKE CONCAT('%.', a.id, '.%')
                            OR c.path LIKE CONCAT('%.', a.id)
                            OR c.path LIKE CONCAT(a.id, '.%')
                            OR c.path = a.id)
                    WHERE
                        is_standard = 1
                        AND is_selectable = 1)
                    AND is_standard = 1 ) temp1
            WHERE
                EXISTS (SELECT
                    1
                FROM
                    (SELECT
                        person_id, visit_occurrence_id, entry_date
                    FROM
                        `cb_search_all_events`
                    WHERE
                        concept_id IN(SELECT
                            DISTINCT c.concept_id
                        FROM
                            `cb_criteria` c
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id
                            FROM
                                `cb_criteria` cr
                            WHERE
                                concept_id IN (312327, 381316)
                                AND full_text LIKE '%_rank1]%'      ) a
                                ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                OR c.path LIKE CONCAT('%.', a.id)
                                OR c.path LIKE CONCAT(a.id, '.%')
                                OR c.path = a.id)
                        WHERE
                            is_standard = 1
                            AND is_selectable = 1)
                        AND is_standard = 1 ) temp2
                WHERE
                    (temp1.person_id = temp2.person_id
                    AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )"


# Demographics ------------------------------------------------------------

#' SQL query to create data frame of patient demographics
#'
#' type: survey
#' @export
sql_demographics <- "
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name
    FROM
        `ds_survey` answer
    WHERE
        (
            question_concept_id IN (SELECT
                DISTINCT concept_id
            FROM
                `cb_criteria` c
            JOIN
                (SELECT
                    CAST(cr.id as string) AS id
                FROM
                    `cb_criteria` cr
                WHERE
                    concept_id IN (1740639)
                    AND domain_id = 'SURVEY') a
                    ON (c.path like CONCAT('%', a.id, '.%'))
            WHERE
                domain_id = 'SURVEY'
                AND type = 'PPI'
                AND subtype = 'QUESTION')
        )
        AND (
            answer.PERSON_ID IN (SELECT
                distinct person_id
            FROM
                `cb_search_person` cb_search_person
            WHERE
                cb_search_person.person_id IN (SELECT
                    criteria.person_id
                FROM
                    (SELECT
                        DISTINCT person_id, entry_date, concept_id
                    FROM
                        `cb_search_all_events`
                    WHERE
                        (concept_id IN(SELECT
                            DISTINCT c.concept_id
                        FROM
                            `cb_criteria` c
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id
                            FROM
                                `cb_criteria` cr
                            WHERE
                                concept_id IN (255848)
                                AND full_text LIKE '%_rank1]%'      ) a
                                ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                OR c.path LIKE CONCAT('%.', a.id)
                                OR c.path LIKE CONCAT(a.id, '.%')
                                OR c.path = a.id)
                        WHERE
                            is_standard = 1
                            AND is_selectable = 1)
                        AND is_standard = 1 )) criteria )
                AND cb_search_person.person_id IN (SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_whole_genome_variant = 1
                UNION
                DISTINCT SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_lr_whole_genome_variant = 1
                UNION
                DISTINCT SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_structural_variant_data = 1 )
                AND cb_search_person.person_id IN (SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_ehr_data = 1 )
                AND cb_search_person.person_id NOT IN (SELECT
                    temp1.person_id
                FROM
                    (SELECT
                        person_id, visit_occurrence_id, entry_date
                    FROM
                        `cb_search_all_events`
                    WHERE
                        concept_id IN(SELECT
                            DISTINCT c.concept_id
                        FROM
                            `cb_criteria` c
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id
                            FROM
                                `cb_criteria` cr
                            WHERE
                                concept_id IN (255848)
                                AND full_text LIKE '%_rank1]%'      ) a
                                ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                OR c.path LIKE CONCAT('%.', a.id)
                                OR c.path LIKE CONCAT(a.id, '.%')
                                OR c.path = a.id)
                        WHERE
                            is_standard = 1
                            AND is_selectable = 1)
                        AND is_standard = 1 ) temp1
                WHERE
                    EXISTS (SELECT
                        1
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (312327, 381316)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp2
                    WHERE
                        (temp1.person_id = temp2.person_id
                        AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
            )"


# Substance use -----------------------------------------------------------

#' SQL query to create data frame of substance use info
#'
#' type: survey
#' @export
sql_substance_use <- "
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name
    FROM
        `ds_survey` answer
    WHERE
        (
            question_concept_id IN (1585650, 1585656, 1585668, 1585680, 1585692, 1585698, 1585857, 1585860, 1585870, 1585873, 1586159, 1586162, 1586166, 1586169, 1586174, 1586177, 1586182, 1586185, 1586190, 1586193, 1586201, 1586207, 903058)
        )
        AND (
            answer.PERSON_ID IN (SELECT
                distinct person_id
            FROM
                `cb_search_person` cb_search_person
            WHERE
                cb_search_person.person_id IN (SELECT
                    criteria.person_id
                FROM
                    (SELECT
                        DISTINCT person_id, entry_date, concept_id
                    FROM
                        `cb_search_all_events`
                    WHERE
                        (concept_id IN(SELECT
                            DISTINCT c.concept_id
                        FROM
                            `cb_criteria` c
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id
                            FROM
                                `cb_criteria` cr
                            WHERE
                                concept_id IN (255848)
                                AND full_text LIKE '%_rank1]%'      ) a
                                ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                OR c.path LIKE CONCAT('%.', a.id)
                                OR c.path LIKE CONCAT(a.id, '.%')
                                OR c.path = a.id)
                        WHERE
                            is_standard = 1
                            AND is_selectable = 1)
                        AND is_standard = 1 )) criteria )
                AND cb_search_person.person_id IN (SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_whole_genome_variant = 1
                UNION
                DISTINCT SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_lr_whole_genome_variant = 1
                UNION
                DISTINCT SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_structural_variant_data = 1 )
                AND cb_search_person.person_id IN (SELECT
                    person_id
                FROM
                    `cb_search_person` p
                WHERE
                    has_ehr_data = 1 )
                AND cb_search_person.person_id NOT IN (SELECT
                    temp1.person_id
                FROM
                    (SELECT
                        person_id, visit_occurrence_id, entry_date
                    FROM
                        `cb_search_all_events`
                    WHERE
                        concept_id IN(SELECT
                            DISTINCT c.concept_id
                        FROM
                            `cb_criteria` c
                        JOIN
                            (SELECT
                                CAST(cr.id as string) AS id
                            FROM
                                `cb_criteria` cr
                            WHERE
                                concept_id IN (255848)
                                AND full_text LIKE '%_rank1]%'      ) a
                                ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                OR c.path LIKE CONCAT('%.', a.id)
                                OR c.path LIKE CONCAT(a.id, '.%')
                                OR c.path = a.id)
                        WHERE
                            is_standard = 1
                            AND is_selectable = 1)
                        AND is_standard = 1 ) temp1
                WHERE
                    EXISTS (SELECT
                        1
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (312327, 381316)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp2
                    WHERE
                        (temp1.person_id = temp2.person_id
                        AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
            )"


# Comorbidities -----------------------------------------------------------

#' SQL query to create data frame of cardiometabolic comorbidities
#'
#' Included comorbidities: CAD, PAD, DM, HLD, hyper-TG, obesity, metabolic syndrome X, PCOS, diabetes insipidus, asthma, COPD, ABPA, liver disease, thyrotoxicosis, goiter
#' type: condition
#' @export
sql_comorbidities <- "
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name
    FROM
        ( SELECT
            *
        FROM
            `condition_occurrence` c_occurrence
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (201820, 255573, 316866, 317009, 317576, 321318, 3654996, 4079876, 4134862, 4194618, 4212540, 4218106, 4242866, 42537741, 432867, 433736, 436670, 439727, 45768671, 46271022, 764123)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                c_occurrence.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) c_occurrence
        LEFT JOIN
            `concept` c_standard_concept
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
        LEFT JOIN
            `concept` c_type
                ON c_occurrence.condition_type_concept_id = c_type.concept_id
        LEFT JOIN
            `visit_occurrence` v
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` visit
                ON v.visit_concept_id = visit.concept_id
        LEFT JOIN
            `concept` c_source_concept
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
        LEFT JOIN
            `concept` c_status
                ON c_occurrence.condition_status_concept_id = c_status.concept_id"


# Pneumonia ---------------------------------------------------------------

#' SQL query to create data frame of patients with pneumonia including info about pneumonia type/date of diagnosis
#'
#' type: condition
#' @export
sql_pneumonia <- "
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name
    FROM
        ( SELECT
            *
        FROM
            `condition_occurrence` c_occurrence
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (252351, 252548, 252552, 252655, 252949, 253235, 253790, 254066, 254266, 254677, 255848, 256722, 256723, 257315, 257908, 258061, 258180, 258354, 258785, 259048, 259852, 259992, 260430, 260754, 261053, 261324, 261326, 3661408, 36676238, 36714118, 37016927, 37110292, 37116366, 4021760, 4025165, 4044215, 40479642, 40480033, 40481335, 40481839, 40482061, 40489912, 4049965, 4050869, 4051332, 4051336, 4102253, 4110180, 4110507, 4110509, 4110510, 4112820, 4114030, 4114031, 4116488, 4117114, 4133224, 4138769, 4141619, 4143092, 4145369, 4148529, 4153356, 4169796, 4174281, 4177385, 4190647, 4195014, 4195452, 4200891, 4203846, 4204819, 4218175, 4225318, 4228277, 4231983, 4231990, 4233319, 4236311, 4248807, 4256236, 4257548, 4267135, 4273378, 4284985, 4293463, 4299862, 43020558, 4309106, 4311555, 4322625, 4327820, 4341520, 436145, 437313, 439857, 440431, 443410, 45763749, 45767051, 45769390, 46269693, 46270027, 46270121, 46270318, 46274035, 763012)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                c_occurrence.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) c_occurrence
        LEFT JOIN
            `concept` c_standard_concept
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
        LEFT JOIN
            `concept` c_type
                ON c_occurrence.condition_type_concept_id = c_type.concept_id
        LEFT JOIN
            `visit_occurrence` v
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` visit
                ON v.visit_concept_id = visit.concept_id
        LEFT JOIN
            `concept` c_source_concept
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
        LEFT JOIN
            `concept` c_status
                ON c_occurrence.condition_status_concept_id = c_status.concept_id"


# CV complications --------------------------------------------------------

#' SQL query to create data frame containing info about MI/stroke
#'
#' type: condition
#' @export
sql_cv_complications <- "
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name
    FROM
        ( SELECT
            *
        FROM
            `condition_occurrence` c_occurrence
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (312327, 314666, 36684840, 36716999, 37309626, 381316, 4045736, 4046360, 40479606, 4048809, 4111710, 4111711, 4133004, 4153352, 4159140, 4189462, 4199501, 4270024, 4296653, 43020460, 4310996, 4317150, 4329847, 434376, 43530605, 43531605, 436706, 438170, 438438, 438447, 440417, 441579, 443537, 44782442, 44782769, 45768439, 46270162, 46270163, 603326)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                c_occurrence.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) c_occurrence
        LEFT JOIN
            `concept` c_standard_concept
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
        LEFT JOIN
            `concept` c_type
                ON c_occurrence.condition_type_concept_id = c_type.concept_id
        LEFT JOIN
            `visit_occurrence` v
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` visit
                ON v.visit_concept_id = visit.concept_id
        LEFT JOIN
            `concept` c_source_concept
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
        LEFT JOIN
            `concept` c_status
                ON c_occurrence.condition_status_concept_id = c_status.concept_id"


# Sputum Cx/UAg/MRSA nasal swab -------------------------------------------

#' SQL query to create data frame containing info about sputum Cx, urine Ag testing, and MRSA nasal swab
#'
#' type: measurement
#' @export
sql_sputum_uag <- "
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value
    FROM
        ( SELECT
            *
        FROM
            `measurement` measurement
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (1001618, 1175793, 21492812, 3001533, 3002516, 3003479, 3005263, 3011363, 3012582, 3014371, 3016981, 3017325, 3021320, 3023419, 3025099, 3025233, 3027544, 3027813, 3028099, 3032014, 3033152, 3035835, 3036072, 3037206, 3046861, 3047267, 3048918, 37019527, 37019583, 37020079, 37020216, 37020756, 37020863, 37020919, 37020928, 37020972, 37021074, 37021170, 37021197, 37021272, 37021298, 37021511, 37021563, 37038061, 4015189, 43534061, 43534066, 46236341, 46236374, 757677, 757678)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                measurement.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) measurement
        LEFT JOIN
            `concept` m_standard_concept
                ON measurement.measurement_concept_id = m_standard_concept.concept_id
        LEFT JOIN
            `concept` m_type
                ON measurement.measurement_type_concept_id = m_type.concept_id
        LEFT JOIN
            `concept` m_operator
                ON measurement.operator_concept_id = m_operator.concept_id
        LEFT JOIN
            `concept` m_value
                ON measurement.value_as_concept_id = m_value.concept_id
        LEFT JOIN
            `concept` m_unit
                ON measurement.unit_concept_id = m_unit.concept_id
        LEFT JOIn
            `visit_occurrence` v
                ON measurement.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` m_visit
                ON v.visit_concept_id = m_visit.concept_id
        LEFT JOIN
            `concept` m_source_concept
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id"


# BCx ---------------------------------------------------------------------

#' SQL query to create data frame containing info about BCx
#'
#' type: measurement
#' @export
sql_bcx <- "
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value
    FROM
        ( SELECT
            *
        FROM
            `measurement` measurement
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (1001502, 1001521, 1001537, 1001582, 1001596, 1001625, 1001635, 1001702, 1001735, 1001759, 1001858, 1001898, 1001901, 1001957, 1002093, 1002107, 1002167, 1002265, 1002284, 1175303, 1175589, 1616541, 3005745, 3023368, 36031280, 36031722, 36031851, 36031982, 36032118, 36032369, 36203227, 36203568, 36203569, 36203570, 36203571, 36203572, 36203573, 36203574, 36203575, 36203576, 36203577, 36203579, 36203580, 36203581, 36204423, 36204424, 36204426, 36303327, 36303554, 36304019, 36304109, 36304213, 36304218, 36304425, 36304429, 36304460, 36304580, 36305068, 36305118, 36305839, 36305921, 37019710, 37020473, 37020488, 37020591, 37020771, 37021035, 37021439, 37021509, 37026462, 37027357, 37030347, 37031452, 37037849, 37041354, 37041898, 37050122, 37053006, 37054661, 37059982, 37065041, 37067266, 37071481, 40770955, 4107893, 4252846)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                measurement.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) measurement
        LEFT JOIN
            `concept` m_standard_concept
                ON measurement.measurement_concept_id = m_standard_concept.concept_id
        LEFT JOIN
            `concept` m_type
                ON measurement.measurement_type_concept_id = m_type.concept_id
        LEFT JOIN
            `concept` m_operator
                ON measurement.operator_concept_id = m_operator.concept_id
        LEFT JOIN
            `concept` m_value
                ON measurement.value_as_concept_id = m_value.concept_id
        LEFT JOIN
            `concept` m_unit
                ON measurement.unit_concept_id = m_unit.concept_id
        LEFT JOIn
            `visit_occurrence` v
                ON measurement.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` m_visit
                ON v.visit_concept_id = m_visit.concept_id
        LEFT JOIN
            `concept` m_source_concept
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id"


# Inflammatory markers ----------------------------------------------------

#' SQL query to create data frame containing info about inflammatory markers
#'
#' type: measurement
#' @export
sql_inflammatory_markers <- "
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value
    FROM
        ( SELECT
            *
        FROM
            `measurement` measurement
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (3003309, 3004410, 3005673, 3009966, 3010156, 3013707, 3015183, 3020460, 3028288, 3028437, 3034639, 3038553, 3044491, 3045567, 37026687, 37046277, 37078620, 4012479, 40762352, 40795800, 4132152, 4184637, 4197971, 4208414, 4261983, 42869630, 42870592, 4319466)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                measurement.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) measurement
        LEFT JOIN
            `concept` m_standard_concept
                ON measurement.measurement_concept_id = m_standard_concept.concept_id
        LEFT JOIN
            `concept` m_type
                ON measurement.measurement_type_concept_id = m_type.concept_id
        LEFT JOIN
            `concept` m_operator
                ON measurement.operator_concept_id = m_operator.concept_id
        LEFT JOIN
            `concept` m_value
                ON measurement.value_as_concept_id = m_value.concept_id
        LEFT JOIN
            `concept` m_unit
                ON measurement.unit_concept_id = m_unit.concept_id
        LEFT JOIn
            `visit_occurrence` v
                ON measurement.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` m_visit
                ON v.visit_concept_id = m_visit.concept_id
        LEFT JOIN
            `concept` m_source_concept
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id"


# LDL/A1c -----------------------------------------------------------------

#' SQL query to create data frame containing info about lipids and HgbA1c
#'
#' type: measurement
#' @export
sql_lipids_a1c <- "
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value
    FROM
        ( SELECT
            *
        FROM
            `measurement` measurement
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (3003309, 3004410, 3005446, 3005673, 3007263, 3030989, 3034639, 36032094, 36304734, 40762352, 40782589, 40795800, 4184637, 4197971, 42869630)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                measurement.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) measurement
        LEFT JOIN
            `concept` m_standard_concept
                ON measurement.measurement_concept_id = m_standard_concept.concept_id
        LEFT JOIN
            `concept` m_type
                ON measurement.measurement_type_concept_id = m_type.concept_id
        LEFT JOIN
            `concept` m_operator
                ON measurement.operator_concept_id = m_operator.concept_id
        LEFT JOIN
            `concept` m_value
                ON measurement.value_as_concept_id = m_value.concept_id
        LEFT JOIN
            `concept` m_unit
                ON measurement.unit_concept_id = m_unit.concept_id
        LEFT JOIn
            `visit_occurrence` v
                ON measurement.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` m_visit
                ON v.visit_concept_id = m_visit.concept_id
        LEFT JOIN
            `concept` m_source_concept
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id"


# Abx ---------------------------------------------------------------------

# type: drugs


# Anti-plt ----------------------------------------------------------------

# type: drugs


# Anti-coag ---------------------------------------------------------------

# type: drugs


# Statins -----------------------------------------------------------------

#' SQL query to create data frame containing info on statin use
#'
#' type: drugs
#' @export
sql_statin <- "
    SELECT
        d_exposure.PERSON_ID,
        d_exposure.ROUTE_SOURCE_VALUE,
        d_exposure.DRUG_CONCEPT_ID,
        d_exposure.DRUG_EXPOSURE_START_DATETIME,
        d_exposure.SIG,
        d_exposure.ROUTE_CONCEPT_ID,
        d_exposure.DRUG_SOURCE_CONCEPT_ID,
        d_exposure.DRUG_SOURCE_VALUE,
        d_exposure.LOT_NUMBER,
        d_exposure.VERBATIM_END_DATE,
        d_exposure.REFILLS,
        d_exposure.DAYS_SUPPLY,
        d_exposure.DOSE_UNIT_SOURCE_VALUE,
        d_exposure.QUANTITY,
        d_exposure.DRUG_TYPE_CONCEPT_ID,
        d_exposure.VISIT_OCCURRENCE_ID,
        d_exposure.STOP_REASON,
        d_exposure.DRUG_EXPOSURE_END_DATETIME,
        d_route.concept_name as ROUTE_CONCEPT_NAME,
        d_type.concept_name as DRUG_TYPE_CONCEPT_NAME,
        d_standard_concept.concept_code as STANDARD_CONCEPT_CODE,
        d_standard_concept.concept_name as STANDARD_CONCEPT_NAME,
        d_standard_concept.vocabulary_id as STANDARD_VOCABULARY,
        d_source_concept.vocabulary_id as SOURCE_VOCABULARY,
        d_source_concept.concept_name as SOURCE_CONCEPT_NAME,
        d_source_concept.concept_code as SOURCE_CONCEPT_CODE,
        d_visit.concept_name as VISIT_OCCURRENCE_CONCEPT_NAME
    from
        ( SELECT
            *
        from
            `drug_exposure` d_exposure
        WHERE
            (
                drug_concept_id in  (
                    select
                        distinct ca.descendant_id
                    from
                        `cb_criteria_ancestor` ca
                    join
                        (
                            select
                                distinct c.concept_id
                            from
                                `cb_criteria` c
                            join
                                (
                                    select
                                        cast(cr.id as string) as id
                                    from
                                        `cb_criteria` cr
                                    where
                                        domain_id = 'DRUG'
                                        and is_standard = 1
                                        and concept_id in (
                                            1545958, 1551860, 1549686, 1510813, 1592085, 40165636, 1539403, 1592180
                                        )
                                        and is_selectable = 1
                                        and full_text like '%[drug_rank1]%'
                                ) a
                                    on (
                                        c.path like concat('%.',
                                    a.id,
                                    '.%')
                                    or c.path like concat('%.',
                                    a.id))
                                where
                                    domain_id = 'DRUG'
                                    and is_standard = 1
                                    and is_selectable = 1
                                ) b
                                    on (
                                        ca.ancestor_id = b.concept_id
                                    )
                            )
                        )
                ) d_exposure
        LEFT JOIN
            `concept` d_route
                on d_exposure.ROUTE_CONCEPT_ID = d_route.CONCEPT_ID
        LEFT JOIN
            `concept` d_type
                on d_exposure.drug_type_concept_id = d_type.CONCEPT_ID
        left join
            `concept` d_standard_concept
                on d_exposure.DRUG_CONCEPT_ID = d_standard_concept.CONCEPT_ID
        LEFT JOIN
            `concept` d_source_concept
                on d_exposure.DRUG_SOURCE_CONCEPT_ID = d_source_concept.CONCEPT_ID
        left join
            `visit_occurrence` v
                on d_exposure.VISIT_OCCURRENCE_ID = v.VISIT_OCCURRENCE_ID
        LEFT JOIN
            `concept` d_visit
                on v.VISIT_CONCEPT_ID = d_visit.CONCEPT_ID"

# Steroids ----------------------------------------------------------------

# type: drugs

# Troponin ----------------------------------------------------------------

#' SQL query to create data frame containing troponin values
#'
#' type: measurement
#' @export
sql_tn <- "
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value
    FROM
        ( SELECT
            *
        FROM
            `measurement` measurement
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id
                FROM
                    `cb_criteria` c
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id
                    FROM
                        `cb_criteria` cr
                    WHERE
                        concept_id IN (37073332, 40776031)
                        AND full_text LIKE '%_rank1]%'      ) a
                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                        OR c.path LIKE CONCAT('%.', a.id)
                        OR c.path LIKE CONCAT(a.id, '.%')
                        OR c.path = a.id)
                WHERE
                    is_standard = 1
                    AND is_selectable = 1)
            )
            AND (
                measurement.PERSON_ID IN (SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (SELECT
                        criteria.person_id
                    FROM
                        (SELECT
                            DISTINCT person_id, entry_date, concept_id
                        FROM
                            `cb_search_all_events`
                        WHERE
                            (concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 )) criteria )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_lr_whole_genome_variant = 1
                    UNION
                    DISTINCT SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_structural_variant_data = 1 )
                    AND cb_search_person.person_id IN (SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_ehr_data = 1 )
                    AND cb_search_person.person_id NOT IN (SELECT
                        temp1.person_id
                    FROM
                        (SELECT
                            person_id, visit_occurrence_id, entry_date
                        FROM
                            `cb_search_all_events`
                        WHERE
                            concept_id IN(SELECT
                                DISTINCT c.concept_id
                            FROM
                                `cb_criteria` c
                            JOIN
                                (SELECT
                                    CAST(cr.id as string) AS id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (255848)
                                    AND full_text LIKE '%_rank1]%'      ) a
                                    ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                    OR c.path LIKE CONCAT('%.', a.id)
                                    OR c.path LIKE CONCAT(a.id, '.%')
                                    OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1)
                            AND is_standard = 1 ) temp1
                    WHERE
                        EXISTS (SELECT
                            1
                        FROM
                            (SELECT
                                person_id, visit_occurrence_id, entry_date
                            FROM
                                `cb_search_all_events`
                            WHERE
                                concept_id IN(SELECT
                                    DISTINCT c.concept_id
                                FROM
                                    `cb_criteria` c
                                JOIN
                                    (SELECT
                                        CAST(cr.id as string) AS id
                                    FROM
                                        `cb_criteria` cr
                                    WHERE
                                        concept_id IN (312327, 381316)
                                        AND full_text LIKE '%_rank1]%'      ) a
                                        ON (c.path LIKE CONCAT('%.', a.id, '.%')
                                        OR c.path LIKE CONCAT('%.', a.id)
                                        OR c.path LIKE CONCAT(a.id, '.%')
                                        OR c.path = a.id)
                                WHERE
                                    is_standard = 1
                                    AND is_selectable = 1)
                                AND is_standard = 1 ) temp2
                        WHERE
                            (temp1.person_id = temp2.person_id
                            AND temp1.entry_date >= DATE_ADD(temp2.entry_date, INTERVAL 1 DAY) )) ) )
                )
            ) measurement
        LEFT JOIN
            `concept` m_standard_concept
                ON measurement.measurement_concept_id = m_standard_concept.concept_id
        LEFT JOIN
            `concept` m_type
                ON measurement.measurement_type_concept_id = m_type.concept_id
        LEFT JOIN
            `concept` m_operator
                ON measurement.operator_concept_id = m_operator.concept_id
        LEFT JOIN
            `concept` m_value
                ON measurement.value_as_concept_id = m_value.concept_id
        LEFT JOIN
            `concept` m_unit
                ON measurement.unit_concept_id = m_unit.concept_id
        LEFT JOIn
            `visit_occurrence` v
                ON measurement.visit_occurrence_id = v.visit_occurrence_id
        LEFT JOIN
            `concept` m_visit
                ON v.visit_concept_id = m_visit.concept_id
        LEFT JOIN
            `concept` m_source_concept
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id"
