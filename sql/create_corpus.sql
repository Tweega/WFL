-- FUNCTION: public.createcorpus(character varying, jsonb, integer, integer)

-- DROP FUNCTION public.createcorpus(character varying, jsonb, integer, integer);

CREATE OR REPLACE FUNCTION public.create_corpus(
	character varying,
	jsonb,
	integer,
	integer)
    RETURNS integer
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE
AS $BODY$

DECLARE
	corpusName ALIAS FOR $1;
	wflJsonb ALIAS FOR $2;
	tokenCount ALIAS FOR $3;
	typeCount ALIAS FOR $4;

BEGIN
	INSERT INTO public.corpora(
	corpus_name, wfl_jsonb, token_count, type_count)
	VALUES (corpusName, wflJsonb, tokenCount, typeCount)
	RETURNING corpus_id;
END;

$BODY$;

ALTER FUNCTION public.createcorpus(character varying, jsonb, integer, integer)
    OWNER TO christopher;
