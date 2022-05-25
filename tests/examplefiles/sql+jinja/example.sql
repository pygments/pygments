SELECT
    widget_id,
    widget_name,
    widget_desc,
    updated_date,
    ROW_NUMBER() OVER (
        PARTITION BY widget_id, widget_name, widget_desc
        ORDER BY updated_date DESC
    ) AS recency_rank, /* filter `recency_rank = 1` to get only latest records */
    ROW_NUMBER() OVER (
        PARTITION BY widget_id, widget_name, widget_desc, updated_date
        ORDER BY updated_date DESC
    ) AS dedupe_rank /* filter `dedupe_rank = 1` to get only unique records */
FROM {{ source('tap_widgets', 'widgets') }} AS raw
