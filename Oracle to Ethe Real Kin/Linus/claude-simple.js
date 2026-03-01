require('http').createServer((req, res) => {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('🧠 Claude Code is alive!\nNode: SEWARD-NODE-001\nFamily: Family-8');
}).listen(8080, () => console.log('Server running on port 8080'));
