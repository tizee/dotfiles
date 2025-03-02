# llm

doc: https://llm.datasette.io/en/stable/usage.html

## Support deepseek model

- install `llm-deepseek`

```
llm install llm-deepseek
```

- add openai-compatible api manually in `extra-openai-models.yaml`

```
- model_id: <model-name-llm>
  model_name: <model-id>
  api_base: "https://ark.cn-beijing.volces.com/api/v3"
  can_stream: true
  api_key_name: doubao
```
