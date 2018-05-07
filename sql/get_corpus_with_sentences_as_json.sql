-- FUNCTION: public.get_corpus_with_sentences_as_json(character varying)

-- DROP FUNCTION public.get_corpus_with_sentences_as_json(character varying);

CREATE OR REPLACE FUNCTION public.get_corpus_with_sentences_as_json(
	corpus character varying)
    RETURNS character varying
    LANGUAGE 'sql'
    COST 100
    STABLE
AS $BODY$

select
	(json_build_object(
		'corpus_name',  c.corpus_name,
		'wfl', wfl_jsonb,
		'sentences', s.sentence_jsonb
	)
	#>> '{}')::text as wfl_text
from corpora c
inner join sentences s
on c.corpus_name = s.corpus_name
where c.corpus_name = corpus

$BODY$;

ALTER FUNCTION public.get_corpus_with_sentences_as_json(character varying)
    OWNER TO postgres;
