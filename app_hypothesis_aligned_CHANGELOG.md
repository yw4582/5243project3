# app_hypothesis_aligned.R — change log

Goal: align the Shiny A/B app with the project hypotheses and survival-based design while preserving current GA4 testing as much as possible.

## 1) Conceptual changes
- The core completion event is now the successful quiz completion itself.
- The ease-of-use question is now optional and appears on the results page, so it no longer contaminates the primary completion metric.
- The session summary now supports survival analysis directly.

## 2) Existing names preserved
These were kept so current GA4 testing should continue to work:
- Events: `ab_assign`, `quiz_session_start`, `question_answered`, `answer_changed`, `quiz_validation_error`, `quiz_complete`, `view_results`, `post_task_rating_submitted`
- Session summary fields: `group`, `device_type`, `duration_sec`, `reached_results`, `progress_depth`, `answered_n`, `revision_count`, `friction_events`, `revision_rate`, `friction_event_rate`, `ease_of_use`

## 3) New GA4 event names added
These were added for hypothesis-aligned analysis:
- `successful_completion`
- `friction_event`
- `quiz_session_censored`
- `quiz_start`
- `session_censored` (CSV event log only)

## 4) New GA4 / event parameters added
- `condition` (duplicate of A/B assignment; clearer for reporting)
- `time_to_event_sec`
- `flow`
- `friction_type`
- `n_missing`
- `progress_depth`

## 5) New session-level fields added to logs/sessions.csv
- `condition`
- `event_ts`
- `completion_ts`
- `time_to_event_sec`
- `time_to_successful_completion_sec`
- `time_among_completers_sec`
- `event_observed`
- `full_completion`
- `completion_status`
- `censor_reason`
- `answered_item_count`
- `n_step_next_b`
- `n_step_back_b`
- `friction_event_count`
- `revision_rate_per_answered_item`
- `friction_event_rate_per_answered_item`

## 6) Important interpretation note
- `reached_results` is now effectively treated as the full-completion indicator for backward compatibility.
- For formal analysis, use `event_observed`, `time_to_event_sec`, `full_completion`, `revision_rate_per_answered_item`, and `friction_event_rate_per_answered_item`.
