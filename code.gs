function doPost(e) {
  try {
    // 1. Lê os dados enviados do formulário (formato JSON)
    var dados = JSON.parse(e.postData.contents);
    
    // 2. Acessa a planilha ativa e a aba 'Respostas'
    var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName("Respostas");
    if (!sheet) {
      sheet = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    }
    
    // 3. Se a planilha estiver vazia, cria o cabeçalho
    if (sheet.getLastRow() === 0) {
      sheet.appendRow([
        "Data/Hora", 
        "Nome",
        "Curso / Área",
        "Exp. SIG/R",
        "SUS_1", "SUS_2", "SUS_3", "SUS_4", "SUS_5",
        "SUS_6", "SUS_7", "SUS_8", "SUS_9", "SUS_10",
        "TAM_PU1", "TAM_PU2",
        "TAM_PEOU1", "TAM_PEOU2",
        "TAM_BI1", "TAM_BI2",
        "Radar Clareza",
        "Bugs/Travamentos",
        "Sugestões Atributos"
      ]);
      sheet.getRange(1, 1, 1, 23).setFontWeight("bold");
    }
    
    // 4. Pega os valores e salva na planilha
    var dataHora = new Date();
    
    var linha = [
      dataHora,                                      // Data/Hora
      dados.nome || "",                              // Nome
      dados.curso_area || "",                        // Curso / Área
      dados.exp_sig || "",                           // Exp. SIG/R
      dados.sus_1 || "", dados.sus_2 || "", dados.sus_3 || "", dados.sus_4 || "", dados.sus_5 || "",
      dados.sus_6 || "", dados.sus_7 || "", dados.sus_8 || "", dados.sus_9 || "", dados.sus_10 || "",
      dados.tam_pu1 || "", dados.tam_pu2 || "",      // TAM PU
      dados.tam_peou1 || "", dados.tam_peou2 || "",  // TAM PEOU
      dados.tam_bi1 || "", dados.tam_bi2 || "",      // TAM BI
      dados.radar_clareza || "",                     // Radar
      dados.bugs || "",                              // Bugs
      dados.sugestoes || ""                          // Sugestões
    ];
    
    sheet.appendRow(linha);
    
    // 5. Retorna uma resposta de sucesso (opcional, mas ajuda a depurar)
    return ContentService.createTextOutput(JSON.stringify({
      status: "sucesso",
      mensagem: "Dados salvos com sucesso!"
    })).setMimeType(ContentService.MimeType.JSON);
    
  } catch (erro) {
    // 6. Em caso de erro, retorna a mensagem para ajudar na depuração
    Logger.log("Erro: " + erro.toString());
    return ContentService.createTextOutput(JSON.stringify({
      status: "erro",
      mensagem: erro.toString()
    })).setMimeType(ContentService.MimeType.JSON);
  }
}
