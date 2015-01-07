// GAME ------------------------------------------------------------------------

// The current game scenario expressed as a string where:
// 
// a) The string ordinal positions correspond to the following scenario
//    positions:
//
//    0 1 2
//    3 4 5
//    6 7 8
//
// b) The characters in the string have the following meaning:
//    
//    1 - 'O' mark in position
//    2 - No mark in position
//    3 - 'X' mark in position
//    (see PositionState enum)
//
var scenario = Array();

// If the user is currently playing as mark 'X'.
var userIsX = false;

// If a play request has been issued but no response has yet been received.
var requestPending = false;

var GameState = {
    X_WINS:     '0',
    O_WINS:     '1',
    DRAW:       '2',
    ACTIVE:     '3'
}
var PositionState = {
    O:          '1',
    FREE:       '2',
    X:          '3'
}

$(document).ready(function() {
    resetGame();
});

function renderScenario() {
    $('.cell').each(function() {
        var idx = getCellIdx($(this));
        switch (scenario[idx]) {
            case PositionState.O:
                $(this)
                    .removeClass('cell-free')
                    .addClass('cell-o')
                    .html('o')
                    .off('click');
                break;
            case PositionState.X: 
                $(this)
                    .removeClass('cell-free')
                    .addClass('cell-x')
                    .html('&times;')
                    .off('click');
                break;
        }
    });
}

// Render the game read-only. Typically in response to the game scenario 
// representing a win or draw.
function disableGame() {
    $('.cell').each(function() {
        $(this).removeClass('cell-free').off('click');
    });
}

function resetGame() {

    // The computer and user switch marks (O -> X, X -> O).
    userIsX = !userIsX;
    $('#player_x').html(userIsX ? "you" : "computer");
    $('#player_o').html(userIsX ? "computer" : "you");

    $('#game_outcome').hide();

    // Reset the current scenario to contain 9 free position.
    for (i=0; i<9; i++) {
        scenario[i] = PositionState.FREE;
    }

    // Return all game cells to starting values.
    $('.cell').each(function() {
        $(this)
            .removeClass('cell-x')
            .removeClass('cell-o')
            .addClass('cell-free')
            .empty()
            .off('click')
            .click(function() {
                userMove(getCellIdx($(this)));
            });
    });

    if (!userIsX) {
        askComputerToMove();
    }
}

// Given a game cell DOM object return its index within the game (0-8).
function getCellIdx(cell) {
    return parseInt(cell.attr('id').split('_')[1]);
}

// End the current game and display an outcome message to the user.
function endGame(outcomeMsg) {
    $('#game_outcome_msg').html(outcomeMsg);
    $('#game_outcome').show();
    disableGame();
}

// User places their mark in the current game scenario.
function userMove(position) {

    // Can't issue a new play request while an existing request is pending.
    if (requestPending) {
        return;
    }
    requestPending = true;

    scenario[position] = userIsX ? PositionState.X : PositionState.O;
    askComputerToMove();
}

// Current game scenario is sent to the server for the computer to place its
// mark.
function askComputerToMove() {
    $.get('/game?q='+scenario.join(''), function(response) {

        response = response.split('');
        var gameState = response.shift();
        scenario = response;

        renderScenario();

        switch (gameState) {
            case GameState.X_WINS:
                endGame(userIsX ? 'OMG! You win.' : 'The computer wins.');
                break;
            case GameState.O_WINS:
                endGame(userIsX ? 'The computer wins.' : 'OMG! You win.');
                break;
            case GameState.DRAW:
                endGame('A draw.');
                break;
        }

        // The user is now free to issue another play request.
        requestPending = false;
    });
}

// GRAPH -----------------------------------------------------------------------

$(document).ready(function() {

    // Percentage of games (out of all possible Tic-Tac-Toe games) that was 
    // lost by the best candidate in each generation.
    var dataset = [
        0.30472103,
        0.29411766,
        0.27192983,
        0.25945947,
        0.20053476,
        0.17475729,
        0.15656565,
        0.12690355,
        0.10497238,
        7.8341015e-2,
        4.1237112e-2,
        3.5353534e-2,
        2.4630541e-2,
        2.116402e-2,
        1.6393442e-2,
        1.1428571e-2,
        1.010101e-2,
        5.1282053e-3,
        5.1020407e-3,
        4.784689e-3,
        0.0
        ];

    var h = 250,            // Graph height
        padBot = 20,        // Bottom margin
        padLft = 35,        // Left margin
        padTop = 10,        // Top margin
        barW = 20,          // Data bar width
        maxY = .4;          // Max. shown y value

    var svg = d3.select("#chart")
        .append("svg")
        .attr("width", padLft+((barW+1)*dataset.length))
        .attr("height", h);

    // Y-axis
    var yAxisData = [0, .1, .2, .3, .4];
    function toYAxisCoord(x) {
        return (h-padBot)-(x*((h-padBot-padTop)/maxY));
    }
    svg.selectAll("line")
        .data(yAxisData.map(toYAxisCoord))
        .enter().append("line")
        .attr("x1", padLft-7)
        .attr("x2", padLft+((barW+1)*dataset.length))
        .attr("y1", function(d) {
            return d;
        })
        .attr("y2", function(d) {
            return d;
        })
        .style("stroke", "#ddd");
    svg.selectAll(".rule")
        .data(yAxisData)
        .enter().append("text")
        .attr("class", "rule")
        .attr("x", 0)
        .attr("y", toYAxisCoord)
        .attr("dy", +3)
        .attr("text-anchor", "right")
        .text(function (d) {
            return String(d3.format('%')(d));
        });

    // X-axis
    var yLabel = Array();
    for (i=0; i<dataset.length; i++) {
        yLabel[i] = i+1;
    }
    svg.selectAll(".rule2")
        .data(yLabel)
        .enter().append("text")
        .attr("class", "rule")
        .attr("x", function(x) {
            return padLft-(barW/2)+(x*(barW+1));
        })
        .attr("y", h)
        .attr("dy", -5)
        .attr("text-anchor", "middle")
        .text(String);
    svg.append("line")
        .attr("x1", padLft)
        .attr("x2", padLft+((barW+1)*dataset.length))
        .attr("y1", h-padBot)
        .attr("y2", h-padBot)
        .style("stroke", "#000");

    // Data
    var initData = Array();
    for (i=0; i<dataset.length; i++) {
        initData[i] = 0;
    }
    svg.selectAll("rect")
       .data(initData)
       .enter()
       .append("rect")
       .attr("x", function(d, i) {
            return padLft+(i*(barW+1));
        })
        .attr("y", function(d) {
            return (h-padBot)-(d*((h-padBot-padTop)/maxY));
        })
        .attr("height", function(d) {
            return d*((h-padBot-padTop)/maxY);
        })
        .attr("width", barW);

    // Animation
    svg.selectAll("rect")
        .data(dataset)
        .transition()
        .duration(750)
        .attr("height", function(d) {
            return d*((h-padBot)/maxY);
        })
        .attr("y", function(d) {
            return (h-padBot)-(d*((h-padBot)/maxY));
        });

});
