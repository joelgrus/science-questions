# python-flask

This is a simple web service built using flask to serve up random questions.

Even though you're not really supposed to, I'm running it in quasi-production
with <a href = "http://gunicorn.org/">gunicorn</a>.

```
EXPORT HOST=localhost
EXPORT PORT=8080
sudo nohup gunicorn -w 4 -b $HOST:$PORT server:app
```
