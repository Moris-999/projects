const express = require('express');
const path = require('path');
const app = express();
const PORT = process.env.PORT || 3000;

// Serve the static HTML/CSS files from the "public" folder
app.use(express.static(path.join(__dirname, 'public')));

// Simple API route for future reservations
app.post('/api/reserve', (req, res) => {
    res.json({ message: "Reservation request received!" });
});

app.listen(PORT, () => {
    console.log(`Server running at http://localhost:${PORT}`);
});